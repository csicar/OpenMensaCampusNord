FROM haskell:8
WORKDIR /opt/server

RUN apt update
RUN apt install -y wget
RUN apt install -y poppler-utils
RUN apt install -y cron
RUN apt install -y ssh

COPY . /opt/server
RUN stack setup
RUN stack build --copy-bins

ENTRYPOINT /opt/server/run.sh
