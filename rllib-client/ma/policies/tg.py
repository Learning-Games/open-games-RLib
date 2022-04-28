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

        assert "agent_id" in args[-1]
        assert args[-1]["agent_id"] in [0, 1], f"Unexpected agent_id: {args[-1]['agent_id']}"
        self._agent_id = args[-1]["agent_id"]

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
        # obs_batch is a tuple containing the moves of the two players in the
        # last round. Player_0's last move is the first part of the tuple while
        # Player_1's last move is the second part of the tuple. The way we
        # currently implement this, in each round one of the two observations
        # is always zero (our own).

        assert len(obs_batch[0]) == 1, f"unexpected length: {len(obs_batch[0])}"
        assert len(obs_batch[1]) == 1, f"unexpected length: {len(obs_batch[1])}"

        other_agent = (1 - self._agent_id)
        return [[self._scale_move(x) for x in obs_batch[other_agent]]], state_batches, {}


# TODO: Do we actually need this function?
always_fraction = lambda fraction, factor: \
    NamedPolicy( name=f"always_fraction @ {fraction, factor}"
               , policy=PolicySpec( policy_class=FractionalReturn
                                  , config={ "fraction": np.array(fraction, dtype=np.float32)
                                           , "factor": np.array(factor, dtype=np.float32)
                                           }
                                  )
               )

# Note: Because we changed the _game_, this is actually always a constant
# _fraction_.

# Play a fixed fraction of the pie, to be used by "player_0" only.
always_constant_0 = lambda x: \
    NamedPolicy( name=f"always_constant_0 @ {x}"
               , policy=PolicySpec( policy_class=ConstantMove
                                  , config={ "move": np.array(x, dtype=np.float32)
                                           , "agent_id": 0
                                           }
                                  )
               )

# Play a fixed fraction of the move the other agent played, to be used by "player_1" only.
always_constant_1 = lambda x: \
    NamedPolicy( name=f"always_constant_1 @ {x}"
               , policy=PolicySpec( policy_class=ConstantMove
                                  , config={ "move": np.array(x, dtype=np.float32)
                                           , "agent_id": 1
                                           }
                                  )
               )

