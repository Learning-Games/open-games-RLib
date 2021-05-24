FROM registry.gitlab.com/pzahn/learning/src:2021-05-24

WORKDIR /build-workdir

################################################################################
# Install the right GHC version and update package index

RUN pwd && stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

RUN stack build --only-snapshot
WORKDIR /
RUN rm -rf /build-workdir
