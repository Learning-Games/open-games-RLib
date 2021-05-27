FROM ghcr.io/philipp-zahn/learning/base:2021-05-27@sha256:eece7f5d0dd72a44e5b3bb078c11492e64a2078f29fa1b9aea05e090eb80b3fc

ADD stack.yaml /repo/stack.yaml
ADD stack.yaml.lock /repo/stack.yaml.lock
ADD package.yaml /repo/package.yaml
ADD src /repo/src
ADD app /repo/app
ADD bench /repo/bench
ADD tests /repo/tests
ADD game /repo/game

WORKDIR /repo/

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

RUN git config --global user.email "game@learning-service" && \
    git config --global user.name "Game learning executable"
