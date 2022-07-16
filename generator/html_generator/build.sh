#!/bin/env bash

docker container run \
    --rm -it \
    --volume $(pwd)/dockers/html-generator-builder/scripts:/scripts \
    --volume $(pwd)/generator/html_generator:/workdir \
    --name html-generator-builder-container \
    html-generator-builder
