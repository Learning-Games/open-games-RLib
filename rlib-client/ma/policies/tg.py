"""
Policies for the Trust game.
"""

from .generic                import NamedPolicy, ConstantMove
from ray.rllib.policy.policy import Policy, PolicySpec
from gym.spaces              import Box
import numpy as np


class FractionalReturn(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "fraction" in args[-1]
        self.fraction = args[-1]["fraction"]

        assert "factor" in args[-1]
        self.factor = args[-1]["factor"]

    def _scale_move(self, move):
        return (move * self.factor * self.fraction)

    # see hack in .generic.ConstantMove
    def _center_move(self, move):
        return move - ( (self.action_space.high + self.action_space.low) / 2 )

    # TODO: ?? Somehow get the last move of player 1, and then
    #       compute factor * that * fraction, and return that.

    # Unclear how to get the last move of player 1, so the safe thing to to is
    # to start from zero and then use the observations to decide.
    def get_initial_state(self):
        return [self._center_move(0)]

    def compute_actions(
        self,
        obs_batch,
        state_batches=None,
        prev_action_batch=None,
        prev_reward_batch=None,
        info_batch=None,
        episodes=None,
        **kwargs
    ):
	# obs_batch is a tuple containing the move of the other player in the
	# last round, with our move being set to 0. We only use this policy for
	# the second player so the second part of the tuple should be zero.

        assert len(obs_batch[0]) == 1, f"unexpected length: {len(obs_batch[0])}"
        return [[self._scale_move(x) for x in obs_batch[0]]], state_batches, {}


always_fraction = lambda fraction, factor: \
    NamedPolicy( name=f"always_fraction @ {fraction, factor}"
               , policy=PolicySpec( policy_class=FractionalReturn
                                  , config={ "fraction": np.array(fraction, dtype=np.float32)
                                           , "factor": np.array(factor, dtype=np.float32)
                                           }
                                  )
               )


always_constant = lambda x: \
    NamedPolicy( name=f"always_constant @ {x}"
               , policy=PolicySpec( policy_class=ConstantMove
                                  , config={ "move": np.array(x, dtype=np.float32) }
                                  )
               )

