#!/bin/env bash

cd $(pwd)/dockers/html-generator-builder

docker image build \
    --tag html-generator-builder \
    --file html_generator_builder.dockerfile \
    .
