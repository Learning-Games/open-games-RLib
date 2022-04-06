# Entrypoint!

"""

    "We" are player 0 i.e. "player_0".
    "They" are player 1 i.e. "player_1".

"""

import ray

from policies import ConstantMove
from ray import tune
from ray.tune.registry import register_env
from ray.rllib.policy.policy import PolicySpec

from env import DiscreteTwoPlayerLearningGamesEnv # DTPLGE for short.


framework = "tf2"
action_space = ["Cooperate", "Defect"]

def main(name, other_players_strategy, learner="PG"):

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
        "framework": framework,
        "env_config": {
            "action_space": action_space
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
            "training_iteration": 10,
            "timesteps_total": 10_000,

            }

    ray.init()

    results = tune.run( learner
                      , name=f"{config['env']}/learned-vs-{other_players_strategy}/{name}" # Determines the folder in ~/ray_results
                      , config=config
                      , stop=stop_conditions
                      , verbose=1
                      )

if __name__ == "__main__":

    main(name="basic", other_players_strategy="learned")
    # main(name="basic", other_players_strategy="always_defect")
    # main(name="basic", other_players_strategy="always_cooperate")


# TODO:
#
#   - [ ] Use the `PolicySpec` to hardcode some specific policies:
#       - [x] AlwaysDefect
#       - [x] AlwaysCoopoerate
#       - [ ] Tit-For-Tat
#   - [ ] Verify it works for Rock-Paper-Scissors
#   - [x] Understand the graphs
#   - [ ] Implement the _shared_ "learned" agent, where the network is the same
#     between both players.
#       - I think we had this working by having only "one" policy in
#       "policies"; maybe there are more explicit ways to share?
