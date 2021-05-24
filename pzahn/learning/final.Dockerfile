FROM registry.gitlab.com/pzahn/learning/base:2021-05-24

COPY . /repo
WORKDIR /repo

WORKDIR /repo/

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

CMD ["stack", "run", "game", "--"]
