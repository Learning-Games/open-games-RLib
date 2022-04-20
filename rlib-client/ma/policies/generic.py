from random                  import choice
from collections             import namedtuple
from ray.rllib.policy.policy import Policy, PolicySpec

NamedPolicy = namedtuple("NamedPolicy", "name policy")

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

    # TODO: Do we really need this?
    def get_initial_state(self):
        move = choice(range(len(self.action_space)))
        return [move]

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
        move = choice(range(len(self.action_space)))
        return [move], state_batches, {}

learned = NamedPolicy( name="learned"
                     , policy=PolicySpec( config={ "model": { "use_lstm": True
                     }}))
