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
            # TODO: This is definitely wrong for action spaces with differen
            # bounds; need to track down the code in ray that it doing this
            # transformation and find the right way to reverse it; for now we
            # just throw an error.
            #
            #   0-1 -> -1, 1
            assert self.action_space.high == 1, "unsupported action space, high must equal 1; review."
            assert self.action_space.low  == 0, "unsupported action space, low must equal 0; review."

            self.move = (move*2) - 1
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
