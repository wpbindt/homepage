#!/usr/bin/env bash

docker image build \
    --file Dockerfile \
    --tag markdown_parser_builder .

