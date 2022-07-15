#!/bin/env bash

docker container run \
    --volume $(pwd)/dockers/markdown_parser_builder/scripts:/scripts \
    --volume $(pwd)/generator/convert_markup:/workdir \
    --rm -it --name build_markdown_parser markdown_parser_builder
