
from typing import Dict, List, Type, Union, Optional

from ray.rllib.agents.trainer            import Trainer, with_common_config
from ray.rllib.evaluation.episode        import Episode
from ray.rllib.evaluation.postprocessing import Postprocessing, compute_advantages
from ray.rllib.models.action_dist        import ActionDistribution
from ray.rllib.models.modelv2            import ModelV2
from ray.rllib.policy                    import Policy
from ray.rllib.policy.sample_batch       import SampleBatch
from ray.rllib.policy.tf_policy_template import build_tf_policy
from ray.rllib.utils.annotations         import override
from ray.rllib.utils.framework           import try_import_tf
from ray.rllib.utils.typing              import TensorType, TrainerConfigDict

tf1, tf, tfv = try_import_tf()

# ——————————————————————————————————————————————————————————————————————————
# DEFAULT CONFIG CONTENTS
# ——————————————————————————————————————————————————————————————————————————

# Add the following (PG-specific) updates to the (base) `Trainer` config in
# rllib/agents/trainer.py (`COMMON_CONFIG` dict).
DEFAULT_CONFIG = with_common_config({
    # No remote workers by default.
    "num_workers": 0,
    # Learning rate.
    "lr": 0.0004,

    # Experimental: By default, switch off preprocessors for PG.
    "_disable_preprocessor_api": True,

    # PG is the first algo (experimental) to not use the distr. exec API
    # anymore.
    "_disable_execution_plan_api": True,
})

# ——————————————————————————————————————————————————————————————————————————
# UTILS
# ——————————————————————————————————————————————————————————————————————————

def post_process_advantages(
        policy: Policy,
        sample_batch: SampleBatch,
        other_agent_batches: Optional[List[SampleBatch]] = None,
        episode: Optional[Episode] = None) -> SampleBatch:
    """Adds the "advantages" column to `sample_batch`.

    Args:
        policy (Policy): The Policy object to do post-processing for.
        sample_batch (SampleBatch): The actual sample batch to post-process.
        other_agent_batches (Optional[List[SampleBatch]]): Optional list of
            other agents' SampleBatch objects.
        episode (Episode): The multi-agent episode object, from which
            `sample_batch` was generated.

    Returns:
        SampleBatch: The SampleBatch enhanced by the added ADVANTAGES field.
    """

    # Calculates advantage values based on the rewards in the sample batch.
    # The value of the last observation is assumed to be 0.0 (no value function
    # estimation at the end of the sampled chunk).
    return compute_advantages(
        rollout=sample_batch,
        last_r=0.0,
        gamma=policy.config["gamma"],
        use_gae=False,
        use_critic=False)

# ——————————————————————————————————————————————————————————————————————————
# PGTFPolicy
# ——————————————————————————————————————————————————————————————————————————

"""
TensorFlow policy class used for PG.
"""

def pg_tf_loss(
        policy: Policy, model: ModelV2, dist_class: Type[ActionDistribution],
        train_batch: SampleBatch) -> Union[TensorType, List[TensorType]]:
    """The basic policy gradients loss function.

    Args:
        policy (Policy): The Policy to calculate the loss for.
        model (ModelV2): The Model to calculate the loss for.
        dist_class (Type[ActionDistribution]: The action distr. class.
        train_batch (SampleBatch): The training data.

    Returns:
        Union[TensorType, List[TensorType]]: A single loss tensor or a list
            of loss tensors.
    """
    # Pass the training data through our model to get distribution parameters.
    dist_inputs, _ = model(train_batch)

    # Create an action distribution object.
    action_dist = dist_class(dist_inputs, model)

    # Calculate the vanilla PG loss based on:
    # L = -E[ log(pi(a|s)) * A]
    loss = -tf.reduce_mean(
        action_dist.logp(train_batch[SampleBatch.ACTIONS]) * tf.cast(
            train_batch[Postprocessing.ADVANTAGES], dtype=tf.float32))

    policy.policy_loss = loss

    return loss

def pg_loss_stats(policy: Policy,
                  train_batch: SampleBatch) -> Dict[str, TensorType]:
    """Returns the calculated loss in a stats dict.

    Args:
        policy (Policy): The Policy object.
        train_batch (SampleBatch): The data used for training.

    Returns:
        Dict[str, TensorType]: The stats dict.
    """

    return {
        "policy_loss": policy.policy_loss,
    }


# Build a child class of `DynamicTFPolicy`, given the extra options:
# - trajectory post-processing function (to calculate advantages)
# - PG loss function
PGTFPolicy = build_tf_policy(
    name="PGTFPolicy",
    get_default_config=lambda: DEFAULT_CONFIG,
    postprocess_fn=post_process_advantages,
    stats_fn=pg_loss_stats,
    loss_fn=pg_tf_loss)

# ——————————————————————————————————————————————————————————————————————————
# THE ALGORITHM ITSELF
# ——————————————————————————————————————————————————————————————————————————

class CustomPG(Trainer):
    """Policy Gradient (PG) Trainer.

    Defines the distributed Trainer class for policy gradients.
    See `pg_[tf|torch]_policy.py` for the definition of the policy losses for
    TensorFlow and PyTorch.

    Detailed documentation:
    https://docs.ray.io/en/master/rllib-algorithms.html#pg

    Only overrides the default config- and policy selectors
    (`get_default_policy` and `get_default_config`). Utilizes
    the default `execution_plan()` of `Trainer`.
    """

    @classmethod
    @override(Trainer)
    def get_default_config(cls) -> TrainerConfigDict:
        return DEFAULT_CONFIG

    @override(Trainer)
    def get_default_policy_class(self, config) -> Type[Policy]:
        return PGTFPolicy

