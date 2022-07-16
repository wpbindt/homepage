FROM haskell:9.2.3-slim

VOLUME /scripts
VOLUME /workdir

ENTRYPOINT ["/bin/bash", "/scripts/entrypoint.sh"]
