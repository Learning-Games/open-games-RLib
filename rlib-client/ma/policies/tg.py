"""
Policies for the Trust game.
"""

from .generic                import NamedPolicy, ConstantMove
from ray.rllib.policy.policy import Policy, PolicySpec
import numpy as np

always_constant = lambda x: \
    NamedPolicy( name=f"always_constant @ {x}"
               , policy=PolicySpec( policy_class=ConstantMove
                                  , config={ "move": np.array(x, dtype=np.float32) }
                                  )
               )

