# Entrypoint!

import ray

from ray import tune
from ray.tune.registry import register_env

from env import DiscreteTwoPlayerLearningGamesEnv # DTPLGE for short.

def run (name, learner="PG"):

    register_env( "DTPLGE"
                , lambda config: DiscreteTwoPlayerLearningGamesEnv(env_config=config)
                )

    framework = "tf2"
    config = {
        "env": "DTPLGE",
        "framework": framework,
        "env_config": {
            # Note: The order doesn't matter at this point.
            "action_space": ["Cooperate", "Defect"]
            }
        }
    
    stop_conditions = {
            "training_iteration": 10,
            "timesteps_total": 10_000,

            }

    results = tune.run( learner
                      , name=f"{config['env']}/{name}" # Determines the folder in ~/ray_results
                      , config=config
                      , stop=stop_conditions
                      , verbose=1
                      )

if __name__ == "__main__":
    ray.init()

    run(name="basic")
