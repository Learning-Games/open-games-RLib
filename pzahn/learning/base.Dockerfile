FROM ghcr.io/philipp-zahn/learning/src:2021-05-27@sha256:79db509e05d95532ea39a473209f8b61183b7ab39ef7d7c1b6c69d6006a67980

WORKDIR /build-workdir

################################################################################
# Install the right GHC version and update package index

RUN pwd && stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

WORKDIR /build-workdir
RUN stack build --only-snapshot

WORKDIR /
RUN rm -rf /build-workdir
