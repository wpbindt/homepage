# Deploying the website
Either run `make deploy-website` in the root directory, or push to the main branch. In the latter case a github action will run the deploy playbook.

# Rotating ssh keys
To rotate ssh keys, first find the ansible vault password in the Github action secrets of this repo (`Settings>Secrets>Actions>$VAULT_PASS`), and put it in a file called `vault_pass.txt`. Then run
```bash
ssh-keygen -N "" -t rsa -q -C ansible-deploy-key-<CURRENT DATE> ssh_key
ansible-vault encrypt --vault-password-file vault_pass.txt ssh_key
rm vault_pass.txt
```
Now add `ssh_key.pub` to the authorized keys on the server, remove the old public key (recognizable by the `ansible-deploy-key-<SOME OLDER DATE>` comment), and commit `ssh_key` to the repo as `$REPO/ansible/ssh_key.txt.secret`.

