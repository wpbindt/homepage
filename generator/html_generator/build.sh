#!/bin/env bash

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
    --volume $(pwd)/generator/html_generator:/workdir \
    --name html-generator-builder-container \
    html-generator-builder $ARGUMENT
