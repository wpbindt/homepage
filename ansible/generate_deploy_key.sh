#!/usr/bin/env bash
set -eo pipefail

stat vault_pass.txt >> /dev/null
CURRENT_DATE=$(date --iso-8601)
ssh-keygen -N "" -t rsa -q -C ansible-deploy-key-$CURRENT_DATE -f ssh_key.txt.secret <<< y
ansible-vault encrypt --vault-password-file vault_pass.txt ssh_key.txt.secret
mv ssh_key.txt.secret.pub ssh_key.pub.nocommit
rm vault_pass.txt
