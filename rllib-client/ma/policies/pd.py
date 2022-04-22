"""
Policies for the Prisoners Dilemma game.
"""

from .generic                import NamedPolicy, ConstantMove, RandomMove
from ray.rllib.policy.policy import Policy, PolicySpec

class TitForTat(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "initial_move" in args[-1]
        self.initial_move = args[-1]["initial_move"]

    def get_initial_state(self):
        return [self.initial_move]

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
        # last round.  So, we pick player_0's last move (i.e., first part of the
        # tuple).  Note: if we wanted player_0 to play tit-for-tat, this would
        # need to change; we'd need to get the second part of the tuple
        # instead.
        assert len(obs_batch[0]) == 1, f"unexpected length: {len(obs_batch[0])}"
        return obs_batch[0], state_batches, {}


pd_action_space = ["Cooperate", "Defect"]

always_defect = NamedPolicy( name="always_defect"
                           , policy=PolicySpec( policy_class=ConstantMove
                                              , config={ "move": pd_action_space.index("Defect")
                                                       }
                                              )
                           )

always_cooperate = NamedPolicy( name="always_cooperate"
                              , policy=PolicySpec( policy_class=ConstantMove
                                                 , config={ "move": pd_action_space.index("Cooperate")
                                                          }
                                                 )
                              )

tit_for_tat = NamedPolicy( name="tit_for_tat"
                         , policy=PolicySpec( policy_class=TitForTat
                                            , config={ "initial_move": pd_action_space.index("Cooperate")
                                                     }
                                            )
                         )

random_pd_move = NamedPolicy( name="random_pd_move"
                             , policy=PolicySpec( policy_class=RandomMove
                                                , config={"action_space": pd_action_space }
                                                )
                             )

