from ray.rllib.env.multi_agent_env import MultiAgentEnv
from gym.spaces import Discrete, Tuple
from copy import copy
import requests

# Functions for dealing with Haskell.
# No functions! Haskell is a joy.

class DiscreteTwoPlayerLearningGamesEnv(MultiAgentEnv):
    def __init__(self, env_config={}):
        """
            Example of an env_config:
                >>> env_config = { "action_space": ["Cooperate", "Defect"] }

            Another example:
                >>> env_config = { "action_space": ["Rock", "Paper", "Scissors"] }
        """

        assert "action_space" in env_config, "Need a 'action_space' in 'env_config'"

        self.action_space = env_config["action_space"]
        self.action_map   = { i: v for (i, v) in enumerate(self.action_space) }
        self.num_actions  = len(self.action_space)
        self.num_agents   = 2

        # Required by `rllib/env/multi_agent_env.py`
        self._agent_ids   = set([ f"player_{i}" for i in range(self.num_agents) ])

        # TODO: Revisit
        # self.move_of_no_move = self.num_actions

        # We make a discrete choice from our action space
        self.action_space      = Discrete(self.num_actions)

        # Our observations are simply a tuple of:
        #
        #   ( ourAction, theirAction )
        #
        self.observation_space = Tuple( ( Discrete(self.num_actions)
                                      ,   Discrete(self.num_actions)
                                        )
                                      )

        self.reset()


    def reset (self):
        # Prisoners Dilemma:   In the beginning, everyone cooperates.
        # Rock-paper-scissors: In the beginning, everyone plays rock?!
        #
        # Alt. In the beginning, everyone makes the dummy move of no-move.
        #
        # TODO:
        #   - move of no move?
        #   - or start from some specific starting point?
        
        obs = { i: (0, 0) for i in range(self.num_agents) }
        return obs


    def step (self, action_dict):
        # Build the data
        data = { "player1Action": self.action_map[ action_dict[0] ]
               , "player2Action": self.action_map[ action_dict[1] ]
               }

        # Call the webserver
        response = requests.post("http://localhost:3000/play", json=data).json()

        # Compute the rewards
        rewards = {}
        for i in range(self.num_agents):
            rewards[i] = response[f"player{i+1}Payoff"]

        # Observations
        observations = {}
        for i in range(self.num_agents):
            observations[i] = ( action_dict[0], action_dict[1] )

        # Dones
        dones = { i: True for i in range(self.num_agents) }
        dones["__all__"] = True

        # Infos
        infos = { i: {} for i in range(self.num_agents) }

        # Return it all!
        return observations, rewards, dones, infos

