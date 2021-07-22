FROM ubuntu:16.04 as build

RUN apt-get update
RUN apt-get -y install build-essential

COPY src /cron

WORKDIR /cron

RUN make

FROM ubuntu:16.04

RUN apt-get update
RUN apt-get -y install tar

RUN mkdir /var/db
COPY --from=build /cron/cron /bin/cron
COPY backups /etc/cron.d/backups
COPY root /var/spool/cron/crontabs/root
COPY cron.sh /cron.sh

CMD ["/cron.sh"]
