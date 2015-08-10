# Detexify Backend Server implemented in Haskell

## Requirements

- Haskell Platform
- `$ cabal install --only-dependencies`

## Compile

    $ cabal build

## Run

    $ dist/build/detexify-hs-backend/detexify-hs-backend

will run the webserver on port 3000. It will load training data from snapshot.json.

## Docker

    docker build -t kirel/detexify-hs-backend .
    docker push kirel/detexify-hs-backend

## Deploy

    ansible-playbook ansible/deploy.yml -i ansible/inventory

## License

Copyright (c) 2009 Daniel Kirsch, released under the MIT license, see MIT-LICENSE
