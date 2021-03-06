"""
    Build up training configurations for all our games.
"""

from typing                     import Dict
from ray.rllib.agents.callbacks import DefaultCallbacks
from ray.rllib.env              import BaseEnv
from ray.rllib.evaluation       import Episode, RolloutWorker
from ray.rllib.policy           import Policy
from policies.pd                import pd_action_space
from policies.rps               import rps_action_space
from policies.generic           import learned

def dict_to_string (d):
    """ Note: Not recursive; only works for a top-level dictionary. """
    return ",".join( f"{k}={v}" for k,v in d.items() )

def make_monty_hall_config (game_server_url="ws://localhost:3000/simple-monty-hall/play"):
    c = { "env": "MH"
        , "env_config": {"game_server_url": game_server_url}
        , "multiagent": make_multiagent_single_player_config()
        , "callbacks": DefaultCallbacks
        }
    c["env_config"]["name"] = f'{c["env"]}/mh/{dict_to_string(c["env_config"])}/'
    return c

def make_multiagent_single_player_config ():
    def select_policy (agent_id, episode, **kwargs):
        assert agent_id in [ 0 ], f"Unknown player: {agent_id}!"
        return f"player_{agent_id}"

    policies_to_train = ["player_0"]

    multiagent = {
        "policies_to_train": policies_to_train,
        "policy_mapping_fn": select_policy,
        "policies": {"player_0": learned.policy}
        }
    return multiagent

def make_trust_game_config ( policy0
                           , policy1
                           , factor=3
                           , pie=10
                           , game_server_url="ws://localhost:3000/trust-game/play"
                           ):
    c = { "env": "TGE"
        , "env_config":
                { "factor": factor
                , "pie": pie
                , "game_server_url": game_server_url
                }
        , "multiagent": make_multiagent_config(policy0, policy1)
        , "callbacks": DefaultCallbacks
        }
    c["env_config"]["name"] = f'{c["env"]}/tg/player0={policy0.name}/player1={policy1.name}/{dict_to_string(c["env_config"])}/'
    return c

def make_multiagent_config (policy0, policy1):
    def select_policy (agent_id, episode, **kwargs):
        assert agent_id in [ 0, 1 ], f"Unknown player: {agent_id}!"
        return f"player_{agent_id}"

    # Note that if neither of them is "learned" there's no training to do.
    assert "learned" in [policy0.name, policy1.name], "At least one policy must be \"learned\""

    policies_to_train = []
    if policy0.name == "learned":
        policies_to_train = policies_to_train + ["player_0"]
    if policy1.name == "learned":
        policies_to_train = policies_to_train + ["player_1"]

    multiagent = {
        "policies_to_train": policies_to_train,
        "policy_mapping_fn": select_policy,
        "policies": {
            "player_0": policy0.policy,
            "player_1": policy1.policy,
            }
        }
    return multiagent

def make_action_space_config ( policy0
                             , policy1
                             , action_space
                             , episode_length
                             , game_server_url
                             , prefix
                             ):
    c = { "env": "DTPLGE"
        , "env_config":
                { "action_space": action_space
                , "episode_length": episode_length # Used by our callback.
                , "game_server_url": game_server_url
                }
        , "multiagent": make_multiagent_config(policy0, policy1)
        , "callbacks": AverageRewardOverEpisodeLength
        }
    c["env_config"]["name"] = f'{c["env"]}/{prefix}/player0={policy0.name}/player1={policy1.name}/ep_len={episode_length}/'
    return c

def make_pd_config (policy0, policy1, episode_length=10, game_server_url="ws://localhost:3000/prisoners-dilemma/play"):
    return make_action_space_config(policy0, policy1, pd_action_space, episode_length, game_server_url, prefix="pd")

def make_rps_config (policy0, policy1, episode_length=10, game_server_url="ws://localhost:3000/rock-paper-scissors/play"):
    return make_action_space_config(policy0, policy1, rps_action_space, episode_length, game_server_url, prefix="rps")

class AverageRewardOverEpisodeLength(DefaultCallbacks):
    def on_episode_end(
        self,
        *,
        worker: RolloutWorker,
        base_env: BaseEnv,
        policies: Dict[str, Policy],
        episode: Episode,
        env_index: int,
        **kwargs
    ):
        # Because we have variable episode-lengths, but ultimately a 1-step
        # game, we only care about the average reward per step.
        ep_len = worker.env.episode_length
        keys = episode.agent_rewards.keys()
        for k in keys:
            name = k[1]
            episode.custom_metrics[f"{name}_step_average"] = episode.agent_rewards[k] / ep_len
