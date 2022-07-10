FROM cytopia/ansible

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN apt-get update
RUN apk add \
    ssh\
    rsync
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
