#!/bin/env bash
set -eo pipefail

if [ $# -eq 0 ]; then
    ARGUMENT=deploy
else
    ARGUMENT=$1
fi

if [ $ARGUMENT == "shell" ]; then
    INTERACTIVE="-it"
fi

docker container run \
    --rm $INTERACTIVE \
    -e VAULT_PASS=$VAULT_PASS \
    -e BUILD_SHA=$(git rev-parse HEAD) \
    --volume $(pwd)/dockers/ansible/scripts:/scripts \
    --volume $(pwd)/ansible:/workdir \
    --volume $(pwd)/static:/workdir/static \
    --name deploy-container \
    deploy-image $ARGUMENT
