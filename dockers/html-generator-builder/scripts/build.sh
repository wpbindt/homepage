#!/bin/bash

cd /workdir
cabal v2-build
EXECUTABLE_PATH=$(cabal exec which html-generator)
cp "$EXECUTABLE_PATH" /workdir/html-generator
