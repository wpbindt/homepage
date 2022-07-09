#!/bin/sh

echo "$VAULT_PASS" > ~/.vault_pass.txt
mkdir ~/.ssh
ansible-vault view --vault-password-file=~/.vault_pass.txt ansible/ssh_key.txt.secret > ~/.ssh/id_rsa
cat ~/.ssh/id_rsa
chmod 0600 ~/.ssh/id_rsa
cat ~/.ssh/id_rsa
ssh -i ~/.ssh/id_rsa -o "StrictHostKeyChecking=no" wessel@109.237.219.178 


ansible-playbook -e "build_sha=$GITHUB_SHA" --vault-password-file ~/.vault_pass.txt -i ansible/inventory.ini ansible/site.yml

