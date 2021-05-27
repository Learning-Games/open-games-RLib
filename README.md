# Learning Framework

* This repo is an extension of the basic open games engine. It provides a playground for learning games.



# Refactored version

This repo is a refactored and simplified implementation on the basis of [this](https://github.com/jules-hedges/open-game-engine) version by Jules Hedges.

It will serve as a basis for several specialized applications.




# Open games in Haskell

This is a Haskell combinator library implementing open games, a battery of examples, and a code generation tool for making the combinator library practical to work with.

This tool allows modular definition of games, and then checking (but not computing) different types of Nash equilibria. After entering a non-equilibrium, the tool can generate detailed information about deviations.

For background on open games, see these two papers:
* [Compositional game theory](https://arxiv.org/abs/1603.04641) by Ghani, Hedges, Winschel and Zahn
* [Bayesian open games](https://arxiv.org/abs/1910.03656) by Bolt, Hedges and Zahn

I hope that this tool will be usable without in-depth knowledge of how it works or how open games work, but I can't make any promises.

From a user's perspective, the examples in `Examples` are intended to be self-documenting. Studying the modules in that directory will be more effective than any documentation I could write.

When I have time I plan to write a paper describing how the preprocessor works.

Feel free to [contact me](mailto:juleshedges.invariant@gmail.com) (ie. [Jules Hedges](https://julesh.com/)) if you have specific questions. ("Why doesn't this block compile correctly?" is a reasonable question.)

Also contact me if you'd like to contribute! A concrete syntax and parser for blocks is an example of something that would be easy to add for a better programmer than me.

Other contributions not recorded by GitHub (because this is a copy of a private repository): Philipp Zahn, Sjoerd Visscher

## The preprocessor

In order to use the preprocessor, create a value of type `Preprocessor.AbstractSyntax.Block`, and then interactively (in GHCi) apply the function `Preprocessor.Preprocessor.compileBlock`. The resulting `Show` instance will result in a string containing Haskell code. Copy this code from the terminal into a file that makes the appropriate imports from `Engine`.

Examples of blocks can be seen in `Examples`, and in each case the resulting generated code can be seen right after the block definition.

The scoping rules for blocks are quite complicated, and reflect the topological rules for string diagrams of open games:
* Block inputs and line outputs (both covariant and contravariant) are variables, which are brought into scope (I think they could be general patterns, but I haven't tested it properly)
* Covariant inputs of lines are expressions which can contain variables brought into scope by the covariant inputs of the block, and the covariant outputs of any previous line
* The covariant outputs of the block are expressions which can contain variables brought into scope by the covariant inputs of the block and the covariant outputs of any line
* Contravariant inputs of lines are expressions which can contain variables brought into scope by any inputs of the block, the covariant outputs of any line, and the contravariant outputs of any later line
* The contravariant outputs of the block are expressions which can contain variables brought into scope by any inputs of the block and any outputs of any line

(Think: covariant values flow downwards, then turn around and come back upwards. Contravariant values only flow upwards.)

The preprocessor does no scope or type checking, so if you make a mistake then you will get a (probably large) error message when you compile the generated code.

## Using the Template Haskell code generator

In addition to using `Block`, one can use `QBlock` and `QLine` by importing
`Preprocessor.THSyntax` and add `{-# LANGUAGE TemplateHaskell #-}` at the top of the file
as a language pragma. This allows to define blocks _inline_ without copy pasting generated code
through GHCI using a syntax very similar to the one for `Block`. In order to generate the code
for a `QBlock` or a list of `QLine`, you need to call the function `generateGame` at the top
level of your file with a function name given as a string and a list of strings for the
arguments this function will use. The last argument is the `QBlock` that define the game.

This approach as some known limitations:

- All the intermediate terms have to be quoted with `[|…|]` which makes the syntax look really
confusing when they are within lists
- The block context can only return parameters and not expressions.

## Run the code

You can use `stack build` to compile the project, `stack test` will run the tests
`stack ghci` and `stack ghci --test` will run ghci using the main target or the test
targets respectively.

## Docker build

Build an image with the source in it:

    docker image build -f pzahn/learning/src.Dockerfile -t ghcr.io/philipp-zahn/learning/src:2021-05-24 .

Build a base image with stack and haskell dependencies in it:

    docker image build -f pzahn/learning/base.Dockerfile -t ghcr.io/philipp-zahn/learning/base:2021-05-24 pzahn/

Building the final image for running:

    docker image build -f pzahn/learning/final.Dockerfile . -t ghcr.io/philipp-zahn/learning/final:2021-05-24

OK, now you can upload everything:

    docker push ghcr.io/philipp-zahn/learning/src:2021-05-24
    docker push ghcr.io/philipp-zahn/learning/base:2021-05-24
    docker push ghcr.io/philipp-zahn/learning/final:2021-05-24

Ideally, you'll change the date tag when you change an image.

## Running remotely

Example setup on Hetzner:

```
root@Debian-109-buster-64-LAMP ~ # mkdir learning-work
root@Debian-109-buster-64-LAMP ~ # cd learning-work/
root@Debian-109-buster-64-LAMP ~/learning-work # git init
Initialized empty Git repository in /root/learning-work/.git/
root@Debian-109-buster-64-LAMP ~/learning-work # cd
root@Debian-109-buster-64-LAMP ~ # mkdir /tmp/learning-watch
```

Login to docker:

```
root@Debian-109-buster-64-LAMP ~ # docker login ghcr.io -u chrisdone
Password:
WARNING! Your password will be stored unencrypted in /root/.docker/config.json.
Configure a credential helper to remove this warning. See
https://docs.docker.com/engine/reference/commandline/login/#credentials-store

Login Succeeded

```

Run:

```
root@Debian-109-buster-64-LAMP ~ # docker run -d --rm  -v/tmp/learning-watch:/tmp/learning-watch  -v/root/learning-work:/root/learning-work  ghcr.io/philipp-zahn/learning/final:2021-05-25  stack run watch
Unable to find image 'ghcr.io/philipp-zahn/learning/final:2021-05-25' locally
2021-05-25: Pulling from philipp-zahn/learning/final
48839397421a: Pull complete
3dbc469ccfbf: Pull complete
dc3251236bd7: Pull complete
1c33fbfb64b1: Pull complete
ff117b414b3d: Pull complete
f001aadd0134: Pull complete
1c6111b186da: Pull complete
eec295e71cd2: Pull complete
84b40f20ff36: Pull complete
fa4259ee3ff1: Pull complete
0c01acb95cae: Pull complete
afc39123ec89: Pull complete
11ccf541b1f2: Pull complete
39e45e9ff04d: Pull complete
98a912d2b25f: Pull complete
bdd706500afa: Pull complete
d8ab7bc6a84a: Pull complete
3f213ad8da19: Pull complete
Digest: sha256:a01cf4dfc0d4525f25f9755259c698888f4cb7561000018f25005585267859c9
Status: Downloaded newer image for ghcr.io/philipp-zahn/learning/final:2021-05-25
31be233daae8873f391b86fc456107653fc538818ccdf45f825a86b0c7b990f2
```

Note the `-d` for "detached" (daemon) mode.

Check it's up and running:

```
root@Debian-109-buster-64-LAMP ~ # docker ps
CONTAINER ID        IMAGE                                         COMMAND                  CREATED             STATUS              PORTS               NAMES
31be233daae8        ghcr.io/philipp-zahn/learning/final:2021-05-25   "stack run watch"   29 seconds ago      Up 3 seconds                            confident_bartik

root@Debian-109-buster-64-LAMP ~ # docker logs 31be233daae8
[15] 2021-05-25 10:59:18.818424272 UTC: Polling for changes ...

```

# Developing scenarios

Clone the scenarios repo as games/ in the learning/ repository:

    philipp-zahn/learning $ git clone git@github.com:philipp-zahn/learning-run-scenarios.git games

In there you may want to switch to your own personal branch, e.g.

    $ cd games
    $ git checkout -b chris
    $ cd ..

Go back to the root of `learning/`. Check your game compiles:

    $ stack run game check calvano
    [11] 2021-05-27 10:03:52.766389021 UTC: Checking with GHC ...
    [1 of 1] Compiling Main             ( /home/chris/Work/philipp-zahn/learning/games/calvano.hs, /home/chris/Work/philipp-zahn/learning/games/calvano.o )

The calvano example can be found
[here](https://github.com/philipp-zahn/learning-run-scenarios/blob/1fcb3a1462d2721fd3760aba6d8c35539f216b2a/calvano.hs#L1)
(stable link).

Run the game locally (ideally with a small number of iterations, just
to sanity check it):

    [11] 2021-05-27 10:07:07.913013048 UTC: Saved as: calvano-1fcb3a1462d2721fd3760aba6d8c35539f216b2a
    [11] 2021-05-27 10:07:07.913117147 UTC: Compiling with GHC ...
    [1 of 1] Compiling Main             ( /home/chris/Work/philipp-zahn/learning/games/calvano.hs, /home/chris/Work/philipp-zahn/learning/games/calvano.o )
    Linking /home/chris/Work/philipp-zahn/learning/bin/calvano-1fcb3a1462d2721fd3760aba6d8c35539f216b2a ...
    [11] 2021-05-27 10:07:08.836552097 UTC: Running in directory /home/chris/Work/philipp-zahn/learning/results/calvano-1fcb3a1462d2721fd3760aba6d8c35539f216b2a/...
    [11] 2021-05-27 10:07:08.887635791 UTC: Run complete in directory /home/chris/Work/philipp-zahn/learning/results/calvano-1fcb3a1462d2721fd3760aba6d8c35539f216b2a/

Inspect the results under `results/thename-thehash`:

    $ ls results/calvano-1fcb3a1462d2721fd3760aba6d8c35539f216b2a/
    parameters.csv  qValues.json  stats  stderr  stdout

Any other files outputted by the game will be in this directory, see
`parameters.csv` and `qValues.json`. The stats, stderr, stdout are
generated for all runs.

Upload the game to be run remotely:

    $ stack run game upload calvano
    Connection to server OK.
    [11] 2021-05-27 10:08:41.947158937 UTC: Uploading to service ...
    calvano.hs                                                                          100%  826    29.2KB/s   00:00
    [11] 2021-05-27 10:08:43.089394092 UTC: Uploaded.

The remote server will pick this up in about 10 seconds and execute
it. After however long it takes for your job to complete, you'll have
results.

To see the output, you can dump the logs for the job:

    $ stack run game logs calvano
    Connection to server OK.
    [11] 2021-05-27 10:09:00.469730753 UTC: Logs for job calvano-9b67aad73b7ed0c6c52e47ba794aa05cfb13089a
    [11] 2021-05-27 10:09:00.469888806 UTC: stdout
    output completed
    [11] 2021-05-27 10:09:01.000543019 UTC: stderr
    [11] 2021-05-27 10:09:01.51540859 UTC: stats
    Starting at Thu May 27 10:08:44 UTC 2021
    Successful end at Thu May 27 10:08:44 UTC 2021
    Running time: 71.02 ms
    [11] 2021-05-27 10:09:02.061183606 UTC: Run the following to download the complete directory of the job:

    scp -r learning-service:/root/learning-work/results/calvano-9b67aad73b7ed0c6c52e47ba794aa05cfb13089a .


If you pull the results down with the recommended command, you'll get
a local copy of everything:

    $ scp -r learning-service:/root/learning-work/results/calvano-9b67aad73b7ed0c6c52e47ba794aa05cfb13089a .
    parameters.csv                                                                      100%  540    18.5KB/s   00:00
    stderr                                                                              100%    0     0.0KB/s   00:00
    stdout                                                                              100%   17     0.6KB/s   00:00
    stats                                                                               100%  111     3.8KB/s   00:00
    qValues.json                                                                        100%    2     0.1KB/s   00:00

All runs are recorded:

* Locally, they're committed and pushed to the learning-run-scenarios
  project.
* Remotely, they're also committed and pushed to the
  learning-run-scenarios project, but on a different branch
  (`learning-service`).
