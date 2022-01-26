# Learning Framework

* This repo is an extension of the basic [open games engine](https://github.com/philipp-zahn/open-games-hs). It provides a playground for learning games.

* The underlying repo is a Haskell combinator library implementing _open games_, a battery of examples, and a code generation tool for making the combinator library practical to work with.

If you have questions, drop me (Philipp) a [mail](mailto:philipp.zahn@unisg.ch)!

This repo is work in progress. Expect changes at any time!

# What are open games?

_Open games_ are a mathematical structure allowing you to describe game-theoretical games. _Open-games-hs_
is a framework to write those games in a programmatic way and analyze those games. The framework is
written in Haskell and this allows Open Games to inherit a lot of features from the haskell ecosystem such
as datatypes, functions and the large set of haskell libraries.

_Open-games-hs_ is a framework implementing the theory of _Open games_ with which you can write a program that
describes a game and its players. You can supply strategies for the game and test the game for equilibrium.
If the game does not reach equilibrium, the list of deviations
is printed and the reason why the player want to deviate is recorded. The biggest strength of open games
is the ability to build your game from smaller modular components that you can
swap out or parameterize.

# Modelling in open games

This [tutorial](https://github.com/philipp-zahn/open-games-hs/blob/master/Tutorial/TUTORIAL.md) shows how to use the software for modelling.


# How to install and run open-games-hs

# Running R

Things to change when running:

* Create an ./output dir
* Change the results hash

docker run -v`pwd`/outputs:/outputs -v`pwd`/Rscripts:/Rscripts -v`pwd`/testGame-87cc1567373a3995220c7a62930e070210a77ff4:/experiment/ --rm ghcr.io/learning-games/r:2021-11-11@sha256:08975d657fd2e84e611028460e55bd32484f34479785e7de4cb52b7d1be286e8 R -f /Rscripts/asymlearners_4phases.R

# How to update the R image

In the repo main directory, run the following, with the date changed to the current date:

    docker image build Rscripts/ -f r.Dockerfile -t ghcr.io/learning-games/r:2021-11-23

The date serves as a tag for this version of the image.

Login to the GitHub registry via:

    docker logout ghcr.io # Clears bad state. This has bitten me once.
    docker login ghcr.io

For your password, it should be a GitHub Developer token
(https://github.com/settings/tokens) with access to write:packages
(which implies read:packages).

Now push the image to the registry, also updating the date here too:

    docker push ghcr.io/learning-games/r:2021-11-23

Jump to `.github/workflows/learn-then-analyze.yml` and `analyze.yml`
and update the references to `ghcr.io/learning-games/r:...`. You don't
need to use the `@sha256...` suffix.

# Patching the R image

    time docker image build Rscripts/ -f r-patch.Dockerfile -t ghcr.io/learning-games/r:2021-11-26
