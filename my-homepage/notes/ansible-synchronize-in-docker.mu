** 11-7-2022

I want to run ansible in a Docker container to use it in Github Actions. For some reason, using the synchronize Ansible module doesn't quite work out of the box in Docker. It complains about a missing ssh client. To fix this, we install `openssh` (and `rsync`):
>FROM cytopia/ansible
>
>ENV ANSIBLE_HOST_KEY_CHECKING=False
>
>RUN apk add \
>    openssh\
>    rsync
>COPY entrypoint.sh /entrypoint.sh
>
>ENTRYPOINT ["/entrypoint.sh"]
With the entrypoint script being
>#!/bin/sh
>
>echo "$VAULT_PASS" > ~/.vault_pass.txt
>mkdir ~/.ssh
>ansible-vault view --vault-password-file=~/.vault_pass.txt ansible/ssh_key.txt.secret > ~/.ssh/id_rsa
>chmod 0600 ~/.ssh/id_rsa
>
>ansible-playbook -e "build_sha=$GITHUB_SHA" --key-file=~/.ssh/id_rsa --vault-password-file ~/.vault_pass.txt -i ansible/inventory.ini ansible/site.yml 
The key option to make it work is the `--key-file` one.
