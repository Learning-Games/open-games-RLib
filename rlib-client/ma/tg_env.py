from ray.rllib.env.multi_agent_env import MultiAgentEnv
from gym.spaces import Discrete, Tuple, Box
from copy import copy
import numpy as np
import requests
import json
from websocket import create_connection

# trust-game env

class TrustGameEnv(MultiAgentEnv):
    def __init__(self, env_config={}):
        ...
        self.num_agents   = 2

        assert "pie" in env_config, "Need a 'pie' in 'env_config'"
        assert "factor" in env_config, "Need a 'factor' in 'env_config'"


        # Player 1 "action" is a number between 0 and pie
        # Player 2 "action" is a number between 0 and (factor * pie)
        self.action_space      = Box(low=0, high=factor*pie, shape=(1,), dtype=np.float32)

        # Our observations are simply a tuple of:
        #
        #   ( ourAction, theirAction )
        #
        # self.observation_space = Tuple( ( Discrete(self.num_actions)
        #                               ,   Discrete(self.num_actions)
        #                                 )
        #                               )

        # Required by `rllib/env/multi_agent_env.py`
        self._agent_ids   = set([ f"player_{i}" for i in range(self.num_agents) ])

        # The websocket we read from
        self.ws = None

        self.reset()


    def reset(self):
        if self.ws:
          self.ws.close()
        self.ws = create_connection("ws://localhost:3000/play")


    def step (self, action_dict):
        """
            action_dict :: AgentID -> Action
        """

        self.step_number += 1

        if self.step_number == 1:
            sent_input = action_dict["agent_1"]
            self.ws.send(sent_input)
            rewards = 0

        if self.step_number == 2:
            sent_back_input = action_dict["agent_2"]
            self.ws.send(sent_back_input)

            # Now we can compute the reward
            rewards = self.ws.recv()

        return observations, rewards, dones, infos


