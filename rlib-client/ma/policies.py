from ray.rllib.policy.policy import Policy

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
        # TODO: Implement
        assert "Implement!"


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


