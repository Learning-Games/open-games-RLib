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
from mh_env   import MontyHallEnv

from policies.generic import learned
from policies.rps     import always_rock, random_rps_move
from policies.pd      import ( always_defect
                             , random_pd_move
                             , always_cooperate
                             , tit_for_tat_0
                             , tit_for_tat_1
                             )
from policies.tg      import ( always_constant_0
                             , always_constant_1
                             )
from configs          import ( make_pd_config
                             , make_rps_config
                             , make_trust_game_config
                             , make_monty_hall_config
                             )

framework = "tf2"

register_env("DTPLGE", lambda config: DiscreteTwoPlayerLearningGamesEnv(env_config=config))
register_env("TGE",    lambda config: TrustGameEnv(env_config=config))
register_env("MH",     lambda config: MontyHallEnv(env_config=config))

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

    folder_path = f"learner={learner}/{conf['env_config']['name']}"

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

    timesteps = 25_000

    train( make_monty_hall_config(), timesteps_total=timesteps )

    train( make_pd_config(learned, always_defect), timesteps_total=timesteps )
    train( make_pd_config(always_defect, learned), timesteps_total=timesteps )
    train( make_pd_config(learned, random_pd_move), timesteps_total=timesteps )
    train( make_pd_config(random_pd_move, learned), timesteps_total=timesteps )
    train( make_pd_config(learned, random_pd_move, episode_length=100), timesteps_total=timesteps)
    train( make_pd_config(learned, tit_for_tat_1), timesteps_total=timesteps )
    train( make_pd_config(tit_for_tat_0, learned), timesteps_total=timesteps )
    train( make_pd_config(learned, learned, episode_length=1), timesteps_total=timesteps )
    train( make_pd_config(learned, learned, episode_length=10), timesteps_total=timesteps )
    train( make_pd_config(learned, learned, episode_length=100), timesteps_total=timesteps )

    train( make_rps_config(learned, always_rock, episode_length=100), timesteps_total=timesteps)
    train( make_rps_config(always_rock, learned, episode_length=100), timesteps_total=timesteps)
    train( make_rps_config(learned, random_rps_move, episode_length=10), timesteps_total=timesteps)
    train( make_rps_config(random_rps_move, learned, episode_length=10), timesteps_total=timesteps)
    train( make_rps_config(learned, random_rps_move, episode_length=100), timesteps_total=timesteps)
    train( make_rps_config(learned, learned, episode_length=1), timesteps_total=timesteps)
    train( make_rps_config(learned, learned, episode_length=10), timesteps_total=timesteps)
    train( make_rps_config(learned, learned, episode_length=100), timesteps_total=timesteps)

    train( make_trust_game_config(learned, always_constant_1(0), pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(always_constant_1(0), learned, pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(learned, always_constant_1(0.1), pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(always_constant_0(0.1), learned, pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(learned, always_constant_1(0.5), pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(always_constant_0(0.5), learned, pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(learned, always_constant_1(1), pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(always_constant_1(1), learned, pie=10), timesteps_total=timesteps )
    train( make_trust_game_config(learned, learned, pie=10), timesteps_total=timesteps )

    print("done.")

