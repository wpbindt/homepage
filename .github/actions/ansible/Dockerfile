FROM cytopia/ansible

ENV ANSIBLE_HOST_KEY_CHECKING=False
RUN apk add rsync openssh
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
