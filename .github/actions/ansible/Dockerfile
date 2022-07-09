FROM python:3-alpine

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN python3 -m pip install --upgrade paramiko ansible
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
