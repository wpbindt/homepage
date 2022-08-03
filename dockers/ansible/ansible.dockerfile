FROM cytopia/ansible

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN apk add \
    openssh\
    rsync \
    ansible-lint

VOLUME /scripts
VOLUME /workdir
WORKDIR /workdir

ENTRYPOINT ["/bin/sh", "/scripts/entrypoint.sh"]
