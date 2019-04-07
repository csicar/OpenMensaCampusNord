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

# Add crontab file in the cron directory
ADD mensacampusnord-cron /etc/cron.d/mensacampusnord-cron

# Give execution rights on the cron job
RUN chmod 0644 /etc/cron.d/mensacampusnord-cron

# Apply cron job
RUN crontab /etc/cron.d/mensacampusnord-cron

# Create the log file to be able to run tail
RUN touch /var/log/cron.log

CMD tail -f /var/log/cron.log &

# Run the command on container startup
ENTRYPOINT cron  -f 
