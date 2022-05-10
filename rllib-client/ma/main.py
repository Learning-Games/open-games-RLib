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


# TODO: Copied from here:
#   https://github.com/ray-project/ray/blob/master/rllib/examples/custom_keras_model.py

from ray.rllib.models.tf.tf_modelv2 import TFModelV2
from ray.rllib.models               import ModelCatalog
from ray.rllib.models.tf.misc       import normc_initializer

# TODO: god knows how much I hate this dynamic search for the library..
from ray.rllib.utils.framework import try_import_tf
tf1, tf, tfv = try_import_tf()

class MyKerasModel(TFModelV2):
    """Custom model for policy gradient algorithms."""

    def __init__(self, obs_space, action_space, num_outputs, model_config, name):
        super(MyKerasModel, self).__init__(
            obs_space, action_space, num_outputs, model_config, name
        )
        num_outputs = 2 # TODO
        self.inputs = tf.keras.layers.Input(shape=obs_space.shape, name="observations") # obs_space.shape

        layer_1 = tf.keras.layers.Dense(
            128,
            name="my_layer1",
            activation=tf.nn.relu,
            kernel_initializer=normc_initializer(1.0),
        )(self.inputs)
        layer_2 = tf.keras.layers.Dense(
            num_outputs,
            name="my_layer2",
            activation=None,
            kernel_initializer=normc_initializer(0.01),
        )(layer_1)
        layer_out = tf.keras.layers.Dense(
            num_outputs,
            name="my_out",
            activation=None,
            kernel_initializer=normc_initializer(0.01),
        )(layer_2)
        value_out = tf.keras.layers.Dense(
            1,
            name="value_out",
            activation=None,
            kernel_initializer=normc_initializer(0.01),
        )(layer_1)
        self.base_model = tf.keras.Model(self.inputs, [layer_out, value_out])

    def forward(self, input_dict, state, seq_lens):
        model_out, self._value_out = self.base_model(input_dict["obs"])
        return model_out, state

    def value_function(self):
        return tf.reshape(self._value_out, [-1])

    def metrics(self):
        return {"foo": tf.constant(42.0)}


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
    # TODO: Try the "custom" model and see what happens
    "model": { "custom_model": "keras_model" }, # NOTE: currently registered in __init__
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

    # TODO: I wonder whether this really needs to happen after init has been called.
    ModelCatalog.register_custom_model("keras_model", MyKerasModel)

    # TODO: What's the difference between the learner and the model???

    timesteps = 25_000

    # train( make_monty_hall_config(), timesteps_total=timesteps )

    train( make_pd_config(learned, always_defect), timesteps_total=timesteps ) # , learner="DQN" )
    # train( make_pd_config(always_defect, learned), timesteps_total=timesteps )
    # train( make_pd_config(learned, random_pd_move), timesteps_total=timesteps )
    # train( make_pd_config(random_pd_move, learned), timesteps_total=timesteps )
    # train( make_pd_config(learned, random_pd_move, episode_length=100), timesteps_total=timesteps)
    # train( make_pd_config(learned, tit_for_tat_1), timesteps_total=timesteps )
    # train( make_pd_config(tit_for_tat_0, learned), timesteps_total=timesteps )
    # train( make_pd_config(learned, learned, episode_length=1), timesteps_total=timesteps )
    # train( make_pd_config(learned, learned, episode_length=10), timesteps_total=timesteps )
    # train( make_pd_config(learned, learned, episode_length=100), timesteps_total=timesteps )

    # train( make_rps_config(learned, always_rock, episode_length=100), timesteps_total=timesteps)
    # train( make_rps_config(always_rock, learned, episode_length=100), timesteps_total=timesteps)
    # train( make_rps_config(learned, random_rps_move, episode_length=10), timesteps_total=timesteps)
    # train( make_rps_config(random_rps_move, learned, episode_length=10), timesteps_total=timesteps)
    # train( make_rps_config(learned, random_rps_move, episode_length=100), timesteps_total=timesteps)
    # train( make_rps_config(learned, learned, episode_length=1), timesteps_total=timesteps)
    # train( make_rps_config(learned, learned, episode_length=10), timesteps_total=timesteps)
    # train( make_rps_config(learned, learned, episode_length=100), timesteps_total=timesteps)

    # train( make_trust_game_config(learned, always_constant_1(0), pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(always_constant_1(0), learned, pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(learned, always_constant_1(0.1), pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(always_constant_0(0.1), learned, pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(learned, always_constant_1(0.5), pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(always_constant_0(0.5), learned, pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(learned, always_constant_1(1), pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(always_constant_1(1), learned, pie=10), timesteps_total=timesteps )
    # train( make_trust_game_config(learned, learned, pie=10), timesteps_total=timesteps )

    print("done.")

