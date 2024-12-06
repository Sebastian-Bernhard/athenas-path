FROM ubuntu:jammy AS base

# Set env
ENV PATH="/root/.ghcup/bin:${PATH}"
ENV PATH="/root/.local/bin/:${PATH}"
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

# General dependencies
RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y \ 
    build-essential \
    wget

# Haskell deps
RUN apt-get install -y \ 
    curl \
    git \
    libffi-dev \
    libffi8ubuntu1 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5 \
    libnuma1 \
    libnuma-dev 

# Install GHCup
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh \
    # https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#dot
    && . /root/.ghcup/env

# Install stack
RUN ghcup install stack

# Make sure that stack and GHCup agree on binaries 
# see https://www.haskell.org/ghcup/guide/#strategy-1-stack-hooks-new-recommended
RUN mkdir -p ~/.stack/hooks/
RUN curl https://raw.githubusercontent.com/haskell/ghcup-hs/master/scripts/hooks/stack/ghc-install.sh \
    > ~/.stack/hooks/ghc-install.sh
RUN chmod +x ~/.stack/hooks/ghc-install.sh
# hooks are only run when 'system-ghc: false'
RUN stack config set system-ghc false --global

# Helpful tools
RUN apt-get install -y \
    htop \
    ripgrep \
    fzf \
    fd-find \
    bat \
    vim \
    tig \
    python3 \
    && ln -s /usr/bin/fdfind /usr/bin/fd \
    && ln -s /usr/bin/batcat /usr/bin/bat
