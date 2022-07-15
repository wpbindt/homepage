#!/usr/bin/env bash

cd $(pwd)/dockers/markdown_parser_builder

docker image build \
    --file Dockerfile \
    --tag markdown_parser_builder .

