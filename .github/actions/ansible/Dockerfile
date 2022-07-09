FROM python:3-alpine

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN apk add ansible
RUN python3 -m pip install --upgrade paramiko
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
