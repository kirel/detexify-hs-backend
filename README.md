# Detexify Backend Server implemented in Haskell

## Requirements

- Haskell Platform
- `$ cabal install --only-dependencies`

## Compile

    $ cabal build

## Run

    $ dist/build/detexify-hs-backend/detexify-hs-backend

will run the webserver on port 3000. It will load training data from snapshot.json.
