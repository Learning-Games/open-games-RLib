# Workflow

## Configuring the remote machine

### Docker build on local machine

Build an image with the source in it:

    docker image build -f pzahn/learning/src.Dockerfile -t ghcr.io/philipp-zahn/learning/src:2021-06-29v2 .

Build a base image with stack and haskell dependencies in it:

    docker image build -f pzahn/learning/base.Dockerfile -t ghcr.io/philipp-zahn/learning/base:2021-06-29v2 pzahn/

Building the final image for running:

    docker image build -f pzahn/learning/final.Dockerfile . -t ghcr.io/philipp-zahn/learning/final:2021-06-29v2


OK, now you can upload everything:

    docker push ghcr.io/philipp-zahn/learning/src:2021-06-29v2
    docker push ghcr.io/philipp-zahn/learning/base:2021-06-29v2
    docker push ghcr.io/philipp-zahn/learning/final:2021-06-29v2

Ideally, you'll change the date tag when you change an image.

### Setting up the remote machine

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
root@Debian-109-buster-64-LAMP ~ # docker run -d --rm  -v/tmp/learning-watch:/tmp/learning-watch  -v/root/learning-work:/root/learning-work  ghcr.io/philipp-zahn/learning/final:2021-11-22  stack run watch
Unable to find image 'ghcr.io/philipp-zahn/learning/final:2021-05-29v2' locally
2021-05-25: Pulling from philipp-zahn/learning/final
48839397421a: Pull complete
3dbc469ccfbf: Pull complete
dc3251236bd7: Pull complete
1c33fbfb64b1: Pull complete
ff117b414b3d: Pull complete
f001aadd0134: Pull complete
1c6111b186da: Pull complete
eec29v25e71cd2: Pull complete
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

## Developing scenarios

### Create local game

In order to version the different scenarios we run, there is a special repository which contains only the scenarios we run.

To get this running in the learning repo, do the following steps:

Clone the scenarios repo as games/ in the learning/ repository:

    philipp-zahn/learning $ git clone git@github.com:philipp-zahn/learning-run-scenarios.git games

NOTE: Check access conditions, in case permission to access is denied

In there you may want to switch to your own personal branch, e.g.

    $ cd games
    $ git checkout -b philipp
    $ cd ..

NOTE: We keep a branch for each of us. This guarantees we only keep info that does belong there.

### Test and run locally

Go back to the root of `learning/`. Check your game compiles:

    $ stack run game check calvano
    [11] 2021-05-27 10:03:52.766389021 UTC: Checking with GHC ...
    [1 of 1] Compiling Main             ( /home/chris/Work/philipp-zahn/learning/games/calvano.hs, /home/chris/Work/philipp-zahn/learning/games/calvano.o )

The calvano example can be found
[here](https://github.com/philipp-zahn/learning-run-scenarios/blob/1fcb3a1462d2721fd3760aba6d8c35539f216b2a/calvano.hs#L1)
(stable link).

Run the game locally (ideally with a small number of iterations, just
to sanity check it):

    $ stack run game local calvano
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

### Run on the remote machine

NOTE: You need to configure your ssh in order to get access

Make an entry in your ~/.ssh/config like this:

Host learning-service
     IdentityFile _path_to_rsa_file_ (e.g. ~/.ssh/id_rsa)
     HostName 168.119.138.173
     User root



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

# Benchmarking

Note the relevant folder and the `Main.hs` it contains

Command to benchmark a specific game inside that folder:

    stack bench --file-watch --ba PDDeterministicPlayer --profile


# Remote workflow

0. Update the R image and push it

1. Update the Haskell image and push it

2. Update the workflow files.
   0. Include the correct image information
   1. Include the correct experiment to be run
   2. NOTE: both yaml files may need to be updated


## How to update the R image

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
