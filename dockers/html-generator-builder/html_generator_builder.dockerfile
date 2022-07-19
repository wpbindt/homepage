FROM haskell:9.2.3-slim

RUN cabal update

VOLUME /scripts
VOLUME /workdir
WORKDIR /workdir

ENTRYPOINT ["/bin/bash", "/scripts/entrypoint.sh"]
