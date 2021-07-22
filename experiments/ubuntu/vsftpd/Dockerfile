FROM ubuntu:16.04 as build

RUN apt-get update
RUN apt-get -y install build-essential

COPY src /vsftpd

WORKDIR /vsftpd

RUN make

FROM ubuntu:16.04

RUN adduser ftp

COPY --from=build /vsftpd/vsftpd /bin/vsftpd
COPY fs/etc/vsftpd /etc/vsftpd
COPY fs/etc/vsftpd.conf /etc/vsftpd.conf
COPY fs/var/ftp /var/ftp/
COPY vsftpd.sh /vsftpd.sh

RUN mkdir /var/log/ftp/

CMD ["/vsftpd.sh"]
