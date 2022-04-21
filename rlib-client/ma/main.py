# Entrypoint!

"""

    "We" are player 0 i.e. "player_0".
    "They" are player 1 i.e. "player_1".

"""

import os
import ray
from collections import namedtuple
from mergedeep   import merge

from ray               import tune
from ray.tune.registry import register_env

from env      import DiscreteTwoPlayerLearningGamesEnv # DTPLGE for short.
from tg_env   import TrustGameEnv

from policies.generic import learned
from policies.rps     import always_rock, random_rps_move
from policies.pd      import ( always_defect
                             , random_pd_move
                             , always_cooperate
                             , tit_for_tat
                             )
from policies.tg      import ( always_constant
                             , always_fraction
                             )
from configs          import ( make_pd_config
                             , make_rps_config
                             , make_trust_game_config
                             )

# TODO: The trust game is non-symmetric and with our current abstraction we
# cannot train the second player while the first player has a fixed strategy.

framework = "tf2"

register_env("DTPLGE", lambda config: DiscreteTwoPlayerLearningGamesEnv(env_config=config))
register_env("TGE",    lambda config: TrustGameEnv(env_config=config))

base_config = {
    # "gamma": 0.8,
    # "rollout_fragment_length": 500,
    # "train_batch_size": 1000,
    # "metrics_num_episodes_for_smoothing": 200,
    "num_gpus": int(os.environ.get("RLLIB_NUM_GPUS", 0)),
    "framework": framework,
    }

def train(conf, timesteps_total=10_000, learner="PG"):

    # Note: 'merge' does a recursive merge, which is what we want.
    config = merge(base_config, conf)

    folder_path = conf["env_config"]["name"]

    stop_conditions = { "timesteps_total": timesteps_total }

    tune.run( learner
            , name    = folder_path
            , config  = config
            , stop    = stop_conditions
            , verbose = 1
            )

if __name__ == "__main__":
    # Only want to 'init' once.
    ray.init()

    # train( make_pd_config(always_defect) )
    # train( make_pd_config(random_pd_move) )
    # train( make_pd_config(random_pd_move, episode_length=100) )

    # train( make_rps_config(random_rps_move, episode_length=10) )
    # train( make_rps_config(always_rock, episode_length=100) )

    train( make_trust_game_config(always_constant(0), pie=10), timesteps_total=20_000 )
    train( make_trust_game_config(always_constant(0.1), pie=10), timesteps_total=20_000 )
    train( make_trust_game_config(always_constant(0.5), pie=10), timesteps_total=20_000 )
    train( make_trust_game_config(always_constant(1), pie=10), timesteps_total=20_000 )
    train( make_trust_game_config(learned, pie=10), timesteps_total=20_000 )
    # train( make_trust_game_config(always_fraction(0, 3), pie=10) )  # TODO: I don't like passing nameless numbers
