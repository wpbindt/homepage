# Deploying the website
Either run `make deploy-website` in the root directory, or push to the main branch. In the latter case a github action will run the deploy playbook.

# Rotating ssh keys
To rotate ssh keys, first find the ansible vault password in the Github action secrets of this repo (`Settings>Secrets>Actions>$VAULT_PASS`), and put it in a file called `vault_pass.txt` in the `ansible` directory. Then make sure `ansible-vault` is installed and run `ansible/generate_deploy_key.sh`. Now add `ssh_key.pub` to the authorized keys on the server, remove the old public key (recognizable by the `ansible-deploy-key-<SOME OLDER DATE>` comment), and commit `ssh_key` to the repo as `$REPO/ansible/ssh_key.txt.secret`.
