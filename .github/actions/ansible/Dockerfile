FROM cytopia/ansible

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN apk add \
    openssh\
    rsync
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
