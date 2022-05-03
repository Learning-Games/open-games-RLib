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

        assert "agent_id" in args[-1]
        assert args[-1]["agent_id"] in [0, 1], f"Unexpected agent_id: {args[-1]['agent_id']}"
        self._agent_id = args[-1]["agent_id"]

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
        # last round. Player_0's last move is the first part of the tuple while
        # Player_1's last move is the second part of the tuple.

        assert len(obs_batch[0]) == 1, f"unexpected length: {len(obs_batch[0])}"
        assert len(obs_batch[1]) == 1, f"unexpected length: {len(obs_batch[1])}"

        other_agent = (1 - self._agent_id)
        return obs_batch[other_agent], state_batches, {}


pd_action_space = ["Cooperate", "Defect"]

# Policy always defecting; can be used by either "player_0" or "player_1".
always_defect = NamedPolicy( name="always_defect"
                           , policy=PolicySpec( policy_class=ConstantMove
                                              , config={ "move": pd_action_space.index("Defect")
                                                       }
                                              )
                           )

# Policy always cooperating; can be used by either "player_0" or "player_1".
always_cooperate = NamedPolicy( name="always_cooperate"
                              , policy=PolicySpec( policy_class=ConstantMove
                                                 , config={ "move": pd_action_space.index("Cooperate")
                                                          }
                                                 )
                              )

# Tit-for-tat policy, to be used by "player_0" only.
tit_for_tat_0 = NamedPolicy( name="tit_for_tat_0"
                           , policy=PolicySpec( policy_class=TitForTat
                                              , config={ "initial_move": pd_action_space.index("Cooperate")
                                                       , "agent_id": 0
                                                       }
                                              )
                           )

# Tit-for-tat policy, to be used by "player_1" only.
tit_for_tat_1 = NamedPolicy( name="tit_for_tat_1"
                           , policy=PolicySpec( policy_class=TitForTat
                                              , config={ "initial_move": pd_action_space.index("Cooperate")
                                                       , "agent_id": 1
                                                       }
                                              )
                           )

# Policy playing random moves; can be used by either "player_0" or "player_1".
random_pd_move = NamedPolicy( name="random_pd_move"
                             , policy=PolicySpec( policy_class=RandomMove
                                                , config={"action_space": pd_action_space }
                                                )
                             )

