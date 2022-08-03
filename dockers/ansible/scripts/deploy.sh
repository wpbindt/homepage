#!/bin/sh

echo "$VAULT_PASS" > /tmp/.vault_pass.txt
mkdir ~/.ssh
ansible-vault view --vault-password-file=/tmp/.vault_pass.txt ./ssh_key.txt.secret > ~/.ssh/id_rsa
chmod 0600 ~/.ssh/id_rsa

ansible-playbook -e "build_sha=$BUILD_SHA" --vault-password-file /tmp/.vault_pass.txt -i inventory.ini site.yml --key-file=~/.ssh/id_rsa
