#!/bin/env bash
set -eo pipefail

if [ $# -eq 0 ]; then
    ARGUMENT=build
else
    ARGUMENT=$1
fi

if [ $ARGUMENT == "shell" ]; then
    INTERACTIVE="-it"
fi

docker container run \
    --rm $INTERACTIVE \
    --volume $(pwd)/dockers/html-generator-builder/scripts:/scripts \
    --volume $(pwd)/html-generator:/workdir \
    --name html-generator-builder-container \
    html-generator-builder $ARGUMENT
