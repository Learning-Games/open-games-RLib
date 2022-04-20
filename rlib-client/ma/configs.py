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

def make_trust_game_config (policy, factor=3, pie=10):
    c = { "env": "TGE"
        , "env_config":
                { "factor": factor
                , "pie": pie
                }
        , "multiagent": make_multiagent_config(policy)
        }
    c["env_config"]["name"] = f'{c["env"]}/player1={policy.name}/{dict_to_string(c["env_config"])}'
    return c

def make_multiagent_config (player_1_policy):
    def select_policy (agent_id, episode, **kwargs):
        assert agent_id in [ 0, 1 ], f"Unknown player: {agent_id}!"
        return f"player_{agent_id}"

    if player_1_policy.name == "learned":
        policies_to_train = ["player_0", "player_1"]
    else:
        policies_to_train = ["player_0"]

    multiagent = {
        "policies_to_train": policies_to_train,
        "policy_mapping_fn": select_policy,
        "policies": {
            "player_0": learned.policy,
            "player_1": player_1_policy.policy,
            }
        }
    return multiagent

def make_action_space_config (policy, action_space, episode_length):
    c = { "env": "DTPLGE"
        , "env_config":
                { "action_space": action_space
                , "episode_length": episode_length # Used by our callback.
                }
        , "multiagent": make_multiagent_config(policy)
        , "callbacks": AverageRewardOverEpisodeLength
        }
    c["env_config"]["name"] = f'{c["env"]}/player1={policy.name}/ep_len={episode_length}'
    return c

def make_pd_config (policy, episode_length=10):
    return make_action_space_config(policy, pd_action_space, episode_length)

def make_rps_config (policy, episode_length=10):
    return make_action_space_config(policy, rps_action_space, episode_length)

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

