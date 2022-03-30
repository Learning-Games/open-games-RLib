## How to use (via conda) 

From this folder:

```
conda create -n rlib-client python=3.8
conda activate rlib-client
pip install -r requirements.txt
```


### Notes

- We've "connected" Rlib!?

Todo:

- [ ] Make it stop training
- [ ] Flip the payoff so it's a reward (minimise instead of maximise)
- [ ] Think about two-players? Should we learn two agents at once?
- [ ] Metrics/graphs of training?
- [ ] Work out how to get the optimal "strategy" from the result of training?
  - I.e. how to save/use the trained agent.


(To move to a Haskell spot)

- [ ] How to track state in the MonadOptic/Servant?
- [ ] ???
- [ ] Extend to other game examples? Have we done the Iterated Prisoners
      Dilemma (IPD) correctly?

