from random                  import choice
from collections             import namedtuple
from ray.rllib.policy.policy import Policy, PolicySpec
from gym.spaces              import Box

NamedPolicy = namedtuple("NamedPolicy", "name policy")

class ConstantMove(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "move" in args[-1]

        move = args[-1]["move"]

        # NOTE: This is a cheap hack to make sure that the continuous constant
        # result is always in the domain of the (centered?) action space.
        #
        # This is probably broken for more complicated action spaces (i.e.
        # tuples, dicts, ...)
        if isinstance(self.action_space, Box):
            # Oh, that means the action space is a 'Box', so self.move needs
            # to be centered.
            #
            # TODO: This is definitely wrong; need to track down the code in
            # ray that it doing this transformation.
            #
            #   0-1 -> -1, 1
            assert self.action_space.high == 1, "unsupported action space; review."
            assert self.action_space.low  == 0, "unsupported action space; review."
            self.move = (move*2) - 1 # - ( (self.action_space.high + self.action_space.low) )
            # print(f"changed {move} to be {self.move}")
        else:
            # Discrete, it's okay.
            self.move = move

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
        x = state_batches[0], state_batches, {}
        # print(f"x={x}")
        return x


class RandomMove(Policy):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        assert "action_space" in args[-1]
        self._action_space = args[-1]["action_space"]

    def _get_random_move(self):
        return choice(range(len(self._action_space)))

    def get_initial_state(self):
        move = self._get_random_move()
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
        move = self._get_random_move()
        return [move], state_batches, {}

learned = NamedPolicy( name="learned"
                     , policy=PolicySpec( config={ "model": { "use_lstm": True
                     }}))
