"""
Policies for the Rock Paper Scissors game.
"""

from .generic                import NamedPolicy, ConstantMove, RandomMove
from ray.rllib.policy.policy import PolicySpec

rps_action_space = ["Rock", "Paper", "Scissors"]

always_rock = NamedPolicy( name="always_rock"
                         , policy=PolicySpec( policy_class=ConstantMove
                                            , config={ "move": rps_action_space.index("Rock") }
                                            )
                         )

random_rps_move = NamedPolicy( name="random_rps_move"
                             , policy=PolicySpec( policy_class=RandomMove
                                                , config={"action_space": rps_action_space }
                                                )
                             )

