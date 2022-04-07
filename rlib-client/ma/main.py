# Entrypoint!

"""

    "We" are player 0 i.e. "player_0".
    "They" are player 1 i.e. "player_1".

"""

from typing import Dict

import ray

from ray                        import tune
from ray.tune.registry          import register_env
from ray.rllib.policy.policy    import PolicySpec
from ray.rllib.agents.callbacks import DefaultCallbacks
from ray.rllib.policy           import Policy
from ray.rllib.evaluation       import Episode, RolloutWorker
from ray.rllib.env              import BaseEnv

from env import DiscreteTwoPlayerLearningGamesEnv # DTPLGE for short.
from policies import ConstantMove, TitForTat, RandomMove

from collections import namedtuple

framework = "tf2"
pd_action_space = ["Cooperate", "Defect"]
rps_action_space = ["Rock", "Paper", "Scissors"]


# Attach the name of each policy to it, so that we can know whether it's
# "learned" (i.e., the agent using it needs to be trained) or not.
NamedPolicy = namedtuple("NamedPolicy", "name policy")

TwoPlayerGame = namedtuple("TwoPlayerGame", "name action_space")

pd_game = TwoPlayerGame(name="pd", action_space=pd_action_space)
rps_game = TwoPlayerGame(name="rps", action_space=rps_action_space)


# PD policies
# TODO: Consider moving these to a separate file

always_defect = NamedPolicy( name="always_defect"
                           , policy=PolicySpec( policy_class=ConstantMove
                                              , config={ "move": pd_action_space.index("Defect") }
                                              )
                           )

always_cooperate = NamedPolicy( name="always_cooperate"
                              , policy=PolicySpec( policy_class=ConstantMove
                                                 , config={ "move": pd_action_space.index("Cooperate") }
                                                 )
                              )

tit_for_tat = NamedPolicy( name="tit_for_tat"
                         , policy=PolicySpec( policy_class=TitForTat
                                            , config={ "initial_move": pd_action_space.index("Cooperate") }
                                            )
                         )

# RPS policies
# TODO: Consider moving these to a separate file

always_rock = NamedPolicy( name="always_rock"
                         , policy=PolicySpec( policy_class=ConstantMove
                                            , config={ "move": rps_action_space.index("Rock") }
                                            )
                         )

random_rps_move = NamedPolicy( name="random_rps_move"
                             , policy=PolicySpec( policy_class=RandomMove
                                                , config={"action_space": rps_action_space }
                                                )
                             )

# Generic policies
# TODO: Consider moving these to a separate file
learned_policy = NamedPolicy( name="learned"
                            , policy=PolicySpec( config={ "model": { "use_lstm": True }
                                                        , "framework": framework
                                                        }
                                                )
                            )


class CustomCallbacks(DefaultCallbacks):
    def on_episode_end(
        self,
        *,
        worker: RolloutWorker,
        base_env: BaseEnv,
        policies: Dict[str, Policy],
        episode: Episode,
        env_index: int,
        **kwargs
    ):
        # Because we have variable episode-lengths, but ultimately a 1-step
        # game, we only care about the average reward per step.
        ep_len = worker.env.episode_length
        keys = episode.agent_rewards.keys()
        for k in keys:
            name = k[1]
            episode.custom_metrics[f"{name}_step_average"] = episode.agent_rewards[k] / ep_len


def main(game, episode_length, player_1_policy, learner="PG"):

    register_env( "DTPLGE"
                , lambda config: DiscreteTwoPlayerLearningGamesEnv(env_config=config)
                )

    def select_policy (agent_id, episode, **kwargs):
        assert agent_id in [ 0, 1 ], f"Unknown player: {agent_id}!"
        return f"player_{agent_id}"

    if player_1_policy.name == "learned":
        policies_to_train = ["player_0", "player_1"]
    else:
        policies_to_train = ["player_0"]

    config = {
        "env": "DTPLGE",
        "callbacks": CustomCallbacks,
        # "rollout_fragment_length": 10,
        # "train_batch_size": 200,
        # "metrics_num_episodes_for_smoothing": 200,
        "framework": framework,
        "env_config": {
            "action_space": game.action_space,
            "episode_length": episode_length
            },
        "multiagent": {
            "policies_to_train": policies_to_train,
            "policy_mapping_fn": select_policy,
            "policies": {
                "player_0": learned_policy.policy,
                "player_1": player_1_policy.policy,
                }
            }
        }

    stop_conditions = {
            # "training_iteration": 100,
            "timesteps_total": 15_000,
            }


    # For easy identification on TensorBoard.
    folder_path = f"{config['env']}/{game.name}/learned-vs-{player_1_policy.name}/ep_len={episode_length}"

    results = tune.run( learner
                      , name=folder_path
                      , config=config
                      , stop=stop_conditions
                      , verbose=1
                      )

if __name__ == "__main__":
    # Only want to 'init' once.
    ray.init()

    ep_len = 10 # 1

    # main(episode_length=ep_len, game=pd_game, player_1_policy=learned)
    # main(episode_length=ep_len, game=pd_game, player_1_policy=always_defect)
    # main(episode_length=ep_len, game=pd_game, player_1_policy=always_cooperate)
    # main(episode_length=ep_len, game=pd_game, player_1_policy=tit_for_tat)

    # main(episode_length=ep_len, game=rps_game, player_1_policy=learned)
    # main(episode_length=ep_len, game=rps_game, player_1_policy=always_rock)
    main(episode_length=ep_len, game=rps_game, player_1_policy=random_rps_move)



# TODO:
#
#   - [X] Use the `PolicySpec` to hardcode some specific policies:
#       - [x] AlwaysDefect
#       - [x] AlwaysCoopoerate
#       - [x] Tit-For-Tat
#   - [ ] Verify it works for Rock-Paper-Scissors
#       - [x] AlwaysRock
#       - [ ] Learned-vs-Learned?
#   - [ ] Add a "random" policy, just for fun
#   - [x] Understand the graphs
#   - [x] Implement the _shared_ "learned" agent, where the network is the same
#         between both players.
#       - Maybe there is a more explicit way to know if the networks are being
#       shared?
#       - Decided against this way of sharing; it doesn't do anything
#       different, and is too confusing.
#   - [ ] Adjust the 'env' so that there is an option of learning a 'combined'
#         reward? (i.e. when should the learner learn to always cooperate? As
#         it gives the best _total_ reward?).
