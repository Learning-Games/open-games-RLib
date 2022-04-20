"""
Policies for the Trust game.
"""

from .generic                import NamedPolicy, ConstantMove
from ray.rllib.policy.policy import Policy, PolicySpec

always_zero = NamedPolicy( name="always_zero"
                         , policy=PolicySpec( policy_class=ConstantMove
                                            , config={ "move": 666 # 0
                                                     }
                                            )
                         )

