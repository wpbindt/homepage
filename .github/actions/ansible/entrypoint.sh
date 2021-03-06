#!/bin/sh

echo "$VAULT_PASS" > ~/.vault_pass.txt
mkdir ~/.ssh
ansible-vault view --vault-password-file=~/.vault_pass.txt ansible/ssh_key.txt.secret > ~/.ssh/id_rsa
chmod 0600 ~/.ssh/id_rsa

ansible-playbook -e "build_sha=$GITHUB_SHA" --vault-password-file ~/.vault_pass.txt -i ansible/inventory.ini ansible/site.yml --key-file=~/.ssh/id_rsa
