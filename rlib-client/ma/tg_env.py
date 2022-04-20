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
        self.num_agents = 2

        assert "pie" in env_config, "Need a 'pie' in 'env_config'"
        assert "factor" in env_config, "Need a 'factor' in 'env_config'"

        pie    = env_config["pie"]
        factor = env_config["factor"]

        # Player 1 "action" is a number between 0 and pie
        # Player 2 "action" is a number between 0 and (factor * pie)

        # TODO: We could have TWO actions; one for step 1, one for step 2. Is
        #       this a good idea?
        # self.action_space      = Tuple(( Box(low=0, high=pie, shape=(1,), dtype=np.float32) # Step 1.
        #                                , Box(low=0, high=factor*pie, shape=(1,), dtype=np.float32) # Step 2.
        #                               ))

        # Wrong; logically.
        # But doesn't crash the Haskell anymore.
        self.action_space      = Box(low=0, high=pie, shape=(1,), dtype=np.float32)

        # TODO: What are our observations?
        self.observation_space = Tuple(( Box(low=0, high=pie,        shape=(1,), dtype=np.float32)
                                       , Box(low=0, high=factor*pie, shape=(1,), dtype=np.float32)
                                      ))


        # Required by `rllib/env/multi_agent_env.py`
        self._agent_ids   = set([ f"player_{i}" for i in range(self.num_agents) ])

        # The websocket we read from
        self.ws = None

        self.reset()


    def reset(self):
        self.step_number = 0

        if self.ws:
          self.ws.close()

        self.ws = create_connection("ws://localhost:3000/play")
        obs = { i: (0, 0) for i in range(self.num_agents) }
        return obs


    def step (self, action_dict):
        """
            action_dict :: AgentID -> Action
        """
        print(action_dict)

        self.step_number += 1

        # Python is elegant. Please forgive us. Everyone knows you can't just
        # "serialise" "integers".
        encode = lambda a: json.dumps((float(a)))

        if self.step_number == 1:
            # TODO: "Mask" it < pie (not < factor*pie)
            sent_input = action_dict[0][0] # Have to pull out the first element of the size-1 array
            # print(f"Sending {encode(sent_input)}")
            self.ws.send(encode(sent_input))

            # At step 1, nothing is known of rewards; so they are zero.
            rewards = { i: 0 for i in range(self.num_agents) }
            is_done = False

            # TODO: Revisit; this seems horribly wrong.
            observations = {}
            for i in range(self.num_agents):
                observations[i] = ( sent_input, 0 )

        if self.step_number == 2:
            sent_back_input = action_dict[1][0]
            self.ws.send(encode(sent_back_input))

            # Now we can compute the reward
            reward1,reward2 = json.loads(self.ws.recv())

            rewards = { i: r for (i,r) in enumerate([reward1, reward2]) }
            is_done = True

            # TODO: Revisit; this seems horribly wrong.
            observations = {}
            for i in range(self.num_agents):
                observations[i] = ( 0, sent_back_input )

        dones = { i: is_done for i in range(self.num_agents) }
        dones["__all__"] = is_done

        # print(f"obs = {observations}")

        # Infos: Empty.
        infos = { i: {} for i in range(self.num_agents) }

        return observations, rewards, dones, infos


