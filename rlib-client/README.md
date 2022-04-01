## How to use (via conda)

From this folder:

```
conda create -n rlib-client python=3.8
conda activate rlib-client
pip install -r requirements.txt
```


### Notes

- We've "connected" Rlib!?
- If the training speed is too slow; could compare the request/response time
  if we used websockets instead of pure requests.
- Could define an "episode" by a number of iterations, instead of <= 100 years
  in jail.

Todo:

- [x] Make it stop training: set done=true at some point.
- [x] For this game, do we have _any_ observations? (yes: the moves themselves, duh)

- [ ] Metrics/graphs of training (tensorboard)?
      - episode_reward_max

- [ ] What stuff could we track in 'info' ?
- [ ] PD: Get the agent to reach the optimal strategy for the fixed opponent (cooperate, tit-for-tat, etc.).
- [ ] RPS: Get the agent to reach the optimal strategy for the fixed opponent.
- [ ] Get the agent(s) to reach the optimal strategy for the multi-agent version (PD + RPS).

- [ ] Work out how to get the optimal "strategy" from the result of training?
  - I.e. how to save/use the trained agent.

