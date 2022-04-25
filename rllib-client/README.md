# About

This folder contains the [Ray][ray] code that let's us train agents for the
games defined in Haskell.

[ray]: https://docs.ray.io/en/latest/


To test the websockets, it's helpful to have [websocat][websocat].

[websocat]: https://github.com/vi/websocat


# How to Use Interactively

To play any of the games you first need to build and run the game server:
```
> stack build open-games-hs:serve-game
> stack exec -- serve-game
```

Testing it:
```
> curl http://localhost:3000/healthcheck
"Ok!
```

## Prisoners Dilemma

Running a game via `websocat`:
```
> echo '{"player1Action": "Defect", "player2Action": "Defect"}' | websocat ws://localhost:3000/prisoners-dilemma/play
```

## Rock-Paper-Scissors

Running a game via `websocat`:
```
> echo '{"player1Action": "Rock", "player2Action": "Paper"}' | websocat ws://localhost:3000/rock-paper-scissors/play
```

## Trust Game

To run a game, first, start an interactive websocat session:
```
> websocat ws://localhost:3000/trust-game/play
```

Then, input the pie size and the factor:
```
> [10,3]
```

Then, play the first move:
```
> 0.5
true
```

Then, play the second move:
```
> 0.5
true
```

Then, observe the result:
```
[12.5,7.5]
```

The `true` outputs is the server confirming the input is good; and the final
array is: `[player_1_reward, player_2_reward]`.

## Monty Hall (Simple)

To run a game, first, start an interactive websocat session:
```
> websocat ws://localhost:3000/simple-monty-hall/play
```

Then, input the door number that is chosen first (1 - 3):
```
> 3
```

Then, input whether you want to switch door or not (true = yes, false = no):
```
> false
```

Then, observe the result (0.0 for a goat and 10.0 for the car):
```
0.0
```

# Environment: How to use (via conda)

Before training, make sure that the game server is built and up and running:
```
> stack build open-games-hs:serve-game
> stack exec -- serve-game
```

Testing it:
```
> curl http://localhost:3000/healthcheck
"Ok!
```

From this folder:

```
conda create -n rllib-client python=3.8
conda activate rllib-client
pip install -r requirements.txt
```

Then, you can train using rllib by running `main.py` in the `ma` directory:
```
python main.py
```

Observe results via `tensorboard`:
```
cd ~/ray_results && tensorboard --logdir .
```

### Todo

Now:

- [ ] Future directions
  - [ ] Principled approach
  - [ ] "Multi-staged"/Sequential game done via multiple 'opengame's in
         Haskell; and a set of staged actions in Python/rllib.

Later:

- [ ] Clean up the Haskell code?
  - [ ] IO Monad?!
- [ ] Experiment with different network architectures for training


Done:

- [!] Work out how to get the optimal "strategy" from the result of training?
  - I.e. how to save/use the trained agent.
- [!] Monty Hall
  - Opted to investigate 'Trust Game' instead.
- [x] Start running more experiments on a GPU?
- [x] Connected RLLib
- [x] Make it stop training: set done=true at some point.
- [x] For this game, do we have _any_ observations? (yes: the moves themselves, duh)
- [x] Metrics/graphs of training (tensorboard)?
      - episode_reward_max
- [x] PD: Get the agent to reach the optimal strategy for the fixed opponent (cooperate, tit-for-tat, etc.).
  - [x] Cooperate
  - [x] Defect
  - [x] Tit-for-tat
- [x] RPS: Get the agent to reach the optimal strategy for the fixed opponent.
  - [x] Fixed opponent
  - [x] Other strategies?!
- [x] Get the agent(s) to reach the optimal strategy for the multi-agent version (PD + RPS).

