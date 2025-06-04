FROM mcr.microsoft.com/devcontainers/base:debian-11

ENV LANG=C.UTF-8

# common haskell + stack dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        dpkg-dev \
        git \
        gcc \
        gnupg \
        g++ \
        libc6-dev \
        libffi-dev \
        libgmp-dev \
        libnuma-dev \
        libtinfo-dev \
        make \
        netbase \
        xz-utils \
        libncurses-dev \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

ARG CABAL_INSTALL=3.12.1.0
ARG CABAL_INSTALL_RELEASE_KEY=1E07C9A1A3088BAD47F74A3E227EE1942B0BDB95

RUN set -eux; \
    cd /tmp; \
    ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
    CABAL_INSTALL_TAR="cabal-install-$CABAL_INSTALL-$ARCH-linux-deb11.tar.xz"; \
    CABAL_INSTALL_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/$CABAL_INSTALL_TAR"; \
    CABAL_INSTALL_SHA256SUMS_URL="https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/SHA256SUMS"; \
    # sha256 from https://downloads.haskell.org/~cabal/cabal-install-$CABAL_INSTALL/SHA256SUMS
    case "$ARCH" in \
        'aarch64') \
            CABAL_INSTALL_SHA256='c14e8198407f37f7276c77b5cefef60ee6a929b4c22d7316577ce8e2301a758e'; \
            ;; \
        'x86_64') \
            CABAL_INSTALL_SHA256='4f60cf1c72f4ad4d82d668839ac61ae15ae4faf6c4b809395799e8a3ee622051'; \
            ;; \
        *) echo >&2 "error: unsupported architecture '$ARCH'"; exit 1 ;; \
    esac; \
    curl -fSL "$CABAL_INSTALL_URL" -o cabal-install.tar.gz; \
    echo "$CABAL_INSTALL_SHA256 cabal-install.tar.gz" | sha256sum --strict --check; \
    \
    curl -sSLO "$CABAL_INSTALL_SHA256SUMS_URL"; \
    curl -sSLO "$CABAL_INSTALL_SHA256SUMS_URL.sig"; \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$CABAL_INSTALL_RELEASE_KEY"; \
    gpg --batch --verify SHA256SUMS.sig SHA256SUMS; \
    # confirm we are verifying SHA256SUMS that matches the release + sha256
    grep "$CABAL_INSTALL_SHA256  $CABAL_INSTALL_TAR" SHA256SUMS; \
    gpgconf --kill all; \
    \
    tar -xf cabal-install.tar.gz -C /usr/local/bin; \
    \
    rm -rf /tmp/*; \
    \
    cabal --version

ARG GHC=9.10.1
ARG GHC_RELEASE_KEY=FFEB7CE81E16A36B3E2DED6F2DE04D4E97DB64AD

RUN set -eux; \
    cd /tmp; \
    ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
    GHC_URL="https://downloads.haskell.org/~ghc/$GHC/ghc-$GHC-$ARCH-deb11-linux.tar.xz"; \
    # sha256 from https://downloads.haskell.org/~ghc/$GHC/SHA256SUMS
    case "$ARCH" in \
        'aarch64') \
            GHC_SHA256='1db449c445d34779d4deaba22341576f7b512a05b6c2b5cb64f3846d1509714e'; \
            ;; \
        'x86_64') \
            GHC_SHA256='78975575b8125ecf1f50f78b1016b14ea6e87abbf1fc39797af469d029c5d737'; \
            ;; \
        *) echo >&2 "error: unsupported architecture '$ARCH'" ; exit 1 ;; \
    esac; \
    curl -sSL "$GHC_URL" -o ghc.tar.xz; \
    echo "$GHC_SHA256 ghc.tar.xz" | sha256sum --strict --check; \
    \
    GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
    curl -sSL "$GHC_URL.sig" -o ghc.tar.xz.sig; \
    gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$GHC_RELEASE_KEY"; \
    gpg --batch --verify ghc.tar.xz.sig ghc.tar.xz; \
    gpgconf --kill all; \
    \
    tar xf ghc.tar.xz; \
    cd "ghc-$GHC-$ARCH-unknown-linux"; \
    ./configure --prefix "/opt/ghc/$GHC"; \
    make install; \
    \
    rm -rf /tmp/*; \
    \
    "/opt/ghc/$GHC/bin/ghc" --version

# install haskell-language-server
# ARG HLS=2.11.0.0
# ARG HLS_RELEASE_KEY=588764FBE22D19C4
# RUN set -eux; \
#     cd /tmp; \
#     ARCH="$(dpkg-architecture --query DEB_BUILD_GNU_CPU)"; \
#     HLS_URL="https://downloads.haskell.org/~hls/haskell-language-server-$HLS/haskell-language-server-$HLS-$ARCH-linux-ubuntu2004.tar.xz"; \
#     # sha256 from https://downloads.haskell.org/~hls/haskell-language-server-$HLS/SHA256SUMS
#     case "$ARCH" in \
#         'aarch64') \
#             HLS_SHA256='bee95bc91a621d8a2e9a9d86dac28ff839605e87316518dae12c779709bd58f1'; \
#             ;; \
#         'x86_64') \
#             HLS_SHA256='731afff5832397edaebf74b0b8421752735c4b2a8f753b46fbdd63876d5fcdb1'; \
#             ;; \
#         *) echo >&2 "error: unsupported architecture '$ARCH'" ; exit 1 ;; \
#     esac; \
#     curl -sSL "$HLS_URL" -o ghc.tar.xz; \
#     echo "$HLS_SHA256 ghc.tar.xz" | sha256sum --strict --check; \
#     \
#     GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
#     curl -sSL "$HLS_URL.sig" -o ghc.tar.xz.sig; \
#     gpg --batch --keyserver keyserver.ubuntu.com --receive-keys "$HLS_RELEASE_KEY"; \
#     gpg --batch --verify ghc.tar.xz.sig ghc.tar.xz; \
#     gpgconf --kill all; \
#     \
#     tar xf ghc.tar.xz; \
#     cd "ghc-$HLS-$ARCH-unknown-linux"; \
#     ./configure --prefix "/opt/ghc/$HLS"; \
#     make install; \
#     \
#     rm -rf /tmp/*; \
#     \
#     "/opt/ghc/$HLS/bin/ghc" --version


ENV PATH=/root/.cabal/bin:/root/.local/bin:/opt/ghc/${GHC}/bin:$PATH
