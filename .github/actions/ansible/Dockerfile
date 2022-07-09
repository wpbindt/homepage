FROM debian:buster 

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN apt-get install ansible gcc python3-dev libc-dev libffi-dev openssl-dev
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
