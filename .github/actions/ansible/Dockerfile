FROM debian:buster 

ENV ANSIBLE_HOST_KEY_CHECKING=False

RUN apt-get update
RUN apt-get install -y\
    ansible\ 
    gcc\ 
    python3-dev\
    libc-dev\ 
    libffi-dev\ 
    libssl-dev\
    ssh\
    rsync
COPY entrypoint.sh /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
