#!/bin/env sh

if [ $1 == "deploy" ]; then
    echo "Deploying the website"
    sh /scripts/deploy.sh
elif [ $1 == "lint" ]; then
    echo "Linting the playbooks"
    sh /scripts/lint.sh
elif [ $1 == "shell" ]; then
    echo "Starting interactive shell"
    /bin/sh
fi
