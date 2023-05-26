FROM debian:stable-slim

WORKDIR /
COPY . .

# Check that the parallel-parser code was copied to the docker container.
RUN cat README.md

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Berlin

# Install dependencies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential \
        python3 \
        pip

# Install ghcup
RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ARG GHC=9.2.7
ARG CABAL=latest

# Install GHC and cabal
RUN ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

# Install parallel-parser
RUN cabal update
RUN cabal install
RUN whereis parallel-parser

# Run tests.
ENTRYPOINT [ "python3", "futhark-parser-tests" ]