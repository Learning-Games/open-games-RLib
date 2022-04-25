from ray.rllib.env.multi_agent_env import MultiAgentEnv
from gym.spaces import Discrete, Tuple, Box
import numpy as np
import requests
import json
from websocket import create_connection

# monty-hall env

class MontyHallEnv(MultiAgentEnv):
    def __init__(self, env_config={}):
        self.num_agents = 1

        assert "game_server_url" in env_config, "Need a 'game_server_url' in 'env_config'!"

        self.game_server_url = env_config["game_server_url"]

        # We always pass two moves, but depending on which step we are we
        # ignore half the tuple.
        # NOTE: Alternatively: we can have 5 actions (1-3 || False-True) and
        # mask appropriately per step.
        # Discrete(2): False (0), True (1)
        self.action_space      = Tuple((Discrete(3), Discrete(2))) # (door, change)
        self.false_value = 0
        self.true_value  = 1

        # One observation per step; namely the amount that the other person sent.
	# NOTE: We use "3" as the invalid / starting observation. Haskell only
	# returns values in [0..2]
        self.observation_space = Discrete(3 + 1)
        self.default_observation = 3

        # Required by `rllib/env/multi_agent_env.py`
        self._agent_ids   = set([ f"player_{i}" for i in range(self.num_agents) ])

        # The websocket we read from
        self.ws = None

        self.reset()


    def reset(self):
        self.step_number = 0

        if self.ws:
          self.ws.close()
        self.ws = create_connection(self.game_server_url)

        obs = { 0: self.default_observation }
        return obs


    def step (self, action_dict):
        """
            action_dict :: AgentID -> Action
        """
        # print(f"action_dict={action_dict}")

        self.step_number += 1

        # Python is elegant. Please forgive us. Everyone knows you can't just
        # "serialise" "integers".
        encode = lambda a: json.dumps((float(a)))

        if self.step_number == 1:
            door = action_dict[0][0] # Have to pull out the first element (door) of the tuple
            door = door + 1 # 1-based on the Haskell side
            self.ws.send(encode(door))

            # At step 1, nothing is known of rewards; so they are zero.
            rewards = { 0: 0 }

            is_done = False

            opened_goat_door = json.loads(self.ws.recv()) - 1
            observations = { 0: opened_goat_door }

        if self.step_number == 2:
            change_door = action_dict[0][1]
            self.ws.send(json.dumps(bool(change_door == 1)))

            payoff = json.loads(self.ws.recv())
            rewards = { 0: payoff }

            is_done = True

            # TODO: Revisit; this seems horribly wrong.
            observations = { 0: self.default_observation }

        dones = { 0: is_done }
        dones["__all__"] = is_done

        # Infos: Empty.
        infos = { 0: {} }

        return observations, rewards, dones, infos

