#!/bin/env bash

cd /workdir
if [ $1 == "build" ]; then
    echo "Starting to build"
    /scripts/build.sh
elif [ $1 == "test" ]; then
    echo "Running the tests"
    /scripts/test.sh
elif [ $1 == "clean" ]; then
    echo "Cleaning up"
    /scripts/clean.sh
elif [ $1 == "shell" ]; then
    echo "Starting interactive shell"
    /bin/bash
fi
