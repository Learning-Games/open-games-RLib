from ray.rllib.policy.policy import Policy
import random

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


class ConstantMove(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "move" in args[-1]
        self.move = args[-1]["move"]

    def get_initial_state(self):
        return [self.move]

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
        return state_batches[0], state_batches, {}


class RandomMove(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "action_space" in args[-1]
        self.action_space = args[-1]["action_space"]

    # # TODO: Do we really need this?
    # def get_initial_state(self):
    #     return [self.move]

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
        move = random.choice(self.action_space)
        return [move], state_batches, {}


