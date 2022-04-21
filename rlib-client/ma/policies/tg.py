"""
Policies for the Trust game.
"""

from .generic                import NamedPolicy, ConstantMove
from ray.rllib.policy.policy import Policy, PolicySpec
import numpy as np


class FractionalReturn(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "fraction" in args[-1]
        self.fraction = args[-1]["fraction"]

    # TODO: ?? Somehow get the last move of player 1, and then
    #       compute factor * that * fraction, and return that.
    # def get_initial_state(self):
    #     return [self.initial_move]

    # def compute_actions(
    #     self,
    #     obs_batch,
    #     state_batches=None,
    #     prev_action_batch=None,
    #     prev_reward_batch=None,
    #     info_batch=None,
    #     episodes=None,
    #     **kwargs
    # ):
    #     # obs_batch is a tuple containing the moves of the two players in the
    #     # last round.  So, we pick player_0's last move (i.e., first part of the
    #     # tuple).  Note: if we wanted player_0 to play tit-for-tat, this would
    #     # need to change; we'd need to get the second part of the tuple
    #     # instead.
    #     assert len(obs_batch[0]) == 1, f"unexpected length: {len(obs_batch[0])}"
    #     return obs_batch[0], state_batches, {}

# TODO:
# always_fraction = lambda x: \
#     NamedPolicy( name=f"always_fraction @ {x}"
#                , policy=PolicySpec( policy_class=FractionalReturn
#                                   , config={ "fraction": np.array(x, dtype=np.float32) }
#                                   )
#                )


always_constant = lambda x: \
    NamedPolicy( name=f"always_constant @ {x}"
               , policy=PolicySpec( policy_class=ConstantMove
                                  , config={ "move": np.array(x, dtype=np.float32) }
                                  )
               )

