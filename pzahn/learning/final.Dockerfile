FROM registry.gitlab.com/pzahn/learning/base:2021-05-24

COPY . /repo
WORKDIR /repo

WORKDIR /repo/

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

RUN git config --global user.email "game@learning-service" && \
    git config --global user.name "Game learning executable"
