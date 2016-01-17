FROM kobx/ghc-7.8
MAINTAINER Daniel Kirsch
RUN cabal update
ADD detexify-hs-backend.cabal /app/
WORKDIR /app/
RUN cabal sandbox init
RUN cabal install --only-dependencies
ADD src /app/src
RUN cabal build
RUN mv /app/dist/build/detexify-hs-backend/detexify-hs-backend /app/
ADD snapshot.json /app/
EXPOSE 3000
CMD /app/detexify-hs-backend
