#!/bin/env bash

if [ $# -eq 0 ]; then
    ARGUMENT=build
else
    ARGUMENT=$1
fi

docker container run \
    --rm \
    --volume $(pwd)/dockers/html-generator-builder/scripts:/scripts \
    --volume $(pwd)/generator/html_generator:/workdir \
    --name html-generator-builder-container \
    html-generator-builder $ARGUMENT
