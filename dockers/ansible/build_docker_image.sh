#!/bin/env bash

DOCKER_PATH=$(pwd)/dockers/ansible

docker image build \
    --tag deploy-image \
    --file $DOCKER_PATH/ansible.dockerfile \
    $(pwd)/ansible
