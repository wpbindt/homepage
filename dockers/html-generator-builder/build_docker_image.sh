#!/bin/env bash

DOCKER_PATH=$(pwd)/dockers/html-generator-builder

docker image build \
    --tag html-generator-builder \
    --file $DOCKER_PATH/html_generator_builder.dockerfile \
    $(pwd)/generator/html_generator
