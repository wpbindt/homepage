** 9-7-2022

In Ansible, trying to copy a directory and its subdirectories to the server as follows:
>- name: Copy directory and all its contents and subdirectories
>  copy:
>    src: local/directory/
>    dest: /server/directory/
>    owner: root
>    group: root
doesn't work. Instead, use Ansible `synchronize` module, which is a wrapper around `rsync`. It works like this:
>- name: Copy directory and all its contents and subdirectories
>  synchronize:
>    src: local/directory/
>    dest: /server/directory/
>    delete: yes
The delete option removes remote files if they don't exist in the source.
