FROM haskell:9.2.3-slim

RUN cabal update
COPY . /temp
WORKDIR /temp
RUN cabal new-build --only-dependencies /temp/hs-blog.cabal

RUN mkdir /root/.cabal/bin
RUN ln -s $(cabal exec which hspec-discover) /root/.cabal/bin/hspec-discover
RUN rm -rf /temp

VOLUME /scripts
VOLUME /workdir
WORKDIR /workdir

ENTRYPOINT ["/bin/bash", "/scripts/entrypoint.sh"]
