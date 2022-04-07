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
from policies import ConstantMove, TitForTat

framework = "tf2"
action_space = ["Cooperate", "Defect"]

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


def main(name, other_players_strategy, episode_length, learner="PG"):

    register_env( "DTPLGE"
                , lambda config: DiscreteTwoPlayerLearningGamesEnv(env_config=config)
                )

    learned_policy = PolicySpec( config={ "model": { "use_lstm": True }
                                        , "framework": framework
                                        }
                              )

    always_defect = PolicySpec( policy_class=ConstantMove
                              , config={"move": action_space.index("Defect") } 
                              )

    always_cooperate = PolicySpec( policy_class=ConstantMove
                              , config={"move": action_space.index("Cooperate") } 
                              )

    tit_for_tat = PolicySpec( policy_class=TitForTat
                              , config={"initial_move": action_space.index("Cooperate") } 
                              )


    def select_policy (agent_id, episode, **kwargs):
        assert agent_id in [ 0, 1 ], f"Unknown player: {agent_id}!"
        return f"player_{agent_id}"

    policies_to_train = ["player_0"]

    # TODO: Refactor!
    if other_players_strategy == "always_cooperate":
        player_1_policy = always_cooperate

    if other_players_strategy == "tit_for_tat":
        player_1_policy = tit_for_tat
    
    if other_players_strategy == "always_defect":
        player_1_policy = always_defect

    if other_players_strategy == "learned":
        player_1_policy = learned_policy
        policies_to_train.append("player_1")

    config = {
        "env": "DTPLGE",
        "callbacks": CustomCallbacks,
        # "rollout_fragment_length": 10,
        # "train_batch_size": 200,
        # "metrics_num_episodes_for_smoothing": 200,
        "framework": framework,
        "env_config": {
            "action_space": action_space,
            "episode_length": episode_length
            },
        "multiagent": {
            "policies_to_train": policies_to_train,
            "policy_mapping_fn": select_policy,
            "policies": {
                "player_0": learned_policy,
                "player_1": player_1_policy
                }
            }
        }

    stop_conditions = {
            # "training_iteration": 100,
            "timesteps_total": 15_000,
            }


    # For easy identification on TensorBoard.
    folder_path = f"{config['env']}/{name}/learned-vs-{other_players_strategy}/ep_len={episode_length}"

    results = tune.run( learner
                      , name=folder_path
                      , config=config
                      , stop=stop_conditions
                      , verbose=1
                      )

if __name__ == "__main__":
    # Only want to 'init' once.
    ray.init()

    ep_len = 1

    main( name="prisoners-dilemma"
        , other_players_strategy="learned"
        , episode_length=ep_len
        )

    main( name="prisoners-dilemma"
        , other_players_strategy="always_defect"
        , episode_length=ep_len
        )

    main( name="prisoners-dilemma"
        , other_players_strategy="always_cooperate"
        , episode_length=ep_len
        )

# TODO:
#
#   - [ ] Use the `PolicySpec` to hardcode some specific policies:
#       - [x] AlwaysDefect
#       - [x] AlwaysCoopoerate
#       - [ ] Tit-For-Tat
#   - [ ] Verify it works for Rock-Paper-Scissors
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
