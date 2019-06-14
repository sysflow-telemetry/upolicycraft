The manifest file contains a starting point for examining an image:

    [
        {
            "Config": "719cd2e3ed04781b11ed372ec8d712fac66d5b60a6fb6190bf76b7d18cb50105.json",
            "RepoTags": [
                "nginx:latest"
            ],
            "Layers": [
                "a463b6f8359c7ecbbea7459ce4250c631057170989213e27d928b30f16d19b82/layer.tar",
                "531ffb108b7ab61937380949c17ad7cbcc4aa8956b35cdd6825df6de37a7bd74/layer.tar",
                "d4bb1266156923f59e1e53c9a15659975c57474e1ba90f1b4943e73d1c363f43/layer.tar"
            ]
        }
    ]


Image Definition
================

The configuration file contains the definition of the image, among other information.

    "history": [
        {
            "created": "2019-06-10T23:24:23.197964064Z",
            "created_by": "/bin/sh -c #(nop) ADD file:5ffb798d64089418ef4d3a261df5ad7cfa038eb2ef778db2b92604ac87228d99 in / "
        },
        {
            "created": "2019-06-10T23:24:23.42168058Z",
            "created_by": "/bin/sh -c #(nop)  CMD [\"bash\"]",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:02:42.070507226Z",
            "created_by": "/bin/sh -c #(nop)  LABEL maintainer=NGINX Docker Maintainers <docker-maint@nginx.com>",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:02:42.242691331Z",
            "created_by": "/bin/sh -c #(nop)  ENV NGINX_VERSION=1.17.0",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:02:42.398209698Z",
            "created_by": "/bin/sh -c #(nop)  ENV NJS_VERSION=0.3.2",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:02:42.553835599Z",
            "created_by": "/bin/sh -c #(nop)  ENV PKG_RELEASE=1~stretch",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:03:08.921814371Z",
            "created_by": "/bin/sh -c set -x     && addgroup --system --gid 101 nginx     && adduser --system --disabled-login --ingroup nginx --no-create-home --home /nonexistent --gecos \"nginx user\" --shell /bin/false --uid 101 nginx \t&& apt-get update \t&& apt-get install --no-install-recommends --no-install-suggests -y gnupg1 apt-transport-https ca-certificates \t&& \tNGINX_GPGKEY=573BFD6B3D8FBC641079A6ABABF5BD827BD9BF62; \tfound=''; \tfor server in \t\tha.pool.sks-keyservers.net \t\thkp://keyserver.ubuntu.com:80 \t\thkp://p80.pool.sks-keyservers.net:80 \t\tpgp.mit.edu \t; do \t\techo \"Fetching GPG key $NGINX_GPGKEY from $server\"; \t\tapt-key adv --keyserver \"$server\" --keyserver-options timeout=10 --recv-keys \"$NGINX_GPGKEY\" && found=yes && break; \tdone; \ttest -z \"$found\" && echo >&2 \"error: failed to fetch GPG key $NGINX_GPGKEY\" && exit 1; \tapt-get remove --purge --auto-remove -y gnupg1 && rm -rf /var/lib/apt/lists/* \t&& dpkgArch=\"$(dpkg --print-architecture)\" \t&& nginxPackages=\" \t\tnginx=${NGINX_VERSION}-${PKG_RELEASE} \t\tnginx-module-xslt=${NGINX_VERSION}-${PKG_RELEASE} \t\tnginx-module-geoip=${NGINX_VERSION}-${PKG_RELEASE} \t\tnginx-module-image-filter=${NGINX_VERSION}-${PKG_RELEASE} \t\tnginx-module-njs=${NGINX_VERSION}.${NJS_VERSION}-${PKG_RELEASE} \t\" \t&& case \"$dpkgArch\" in \t\tamd64|i386) \t\t\techo \"deb https://nginx.org/packages/mainline/debian/ stretch nginx\" >> /etc/apt/sources.list.d/nginx.list \t\t\t&& apt-get update \t\t\t;; \t\t*) \t\t\techo \"deb-src https://nginx.org/packages/mainline/debian/ stretch nginx\" >> /etc/apt/sources.list.d/nginx.list \t\t\t\t\t\t&& tempDir=\"$(mktemp -d)\" \t\t\t&& chmod 777 \"$tempDir\" \t\t\t\t\t\t&& savedAptMark=\"$(apt-mark showmanual)\" \t\t\t\t\t\t&& apt-get update \t\t\t&& apt-get build-dep -y $nginxPackages \t\t\t&& ( \t\t\t\tcd \"$tempDir\" \t\t\t\t&& DEB_BUILD_OPTIONS=\"nocheck parallel=$(nproc)\" \t\t\t\t\tapt-get source --compile $nginxPackages \t\t\t) \t\t\t\t\t\t&& apt-mark showmanual | xargs apt-mark auto > /dev/null \t\t\t&& { [ -z \"$savedAptMark\" ] || apt-mark manual $savedAptMark; } \t\t\t\t\t\t&& ls -lAFh \"$tempDir\" \t\t\t&& ( cd \"$tempDir\" && dpkg-scanpackages . > Packages ) \t\t\t&& grep '^Package: ' \"$tempDir/Packages\" \t\t\t&& echo \"deb [ trusted=yes ] file://$tempDir ./\" > /etc/apt/sources.list.d/temp.list \t\t\t&& apt-get -o Acquire::GzipIndexes=false update \t\t\t;; \tesac \t\t&& apt-get install --no-install-recommends --no-install-suggests -y \t\t\t\t\t\t$nginxPackages \t\t\t\t\t\tgettext-base \t&& apt-get remove --purge --auto-remove -y apt-transport-https ca-certificates && rm -rf /var/lib/apt/lists/* /etc/apt/sources.list.d/nginx.list \t\t&& if [ -n \"$tempDir\" ]; then \t\tapt-get purge -y --auto-remove \t\t&& rm -rf \"$tempDir\" /etc/apt/sources.list.d/temp.list; \tfi"
        },
        {
            "created": "2019-06-11T00:03:09.904214824Z",
            "created_by": "/bin/sh -c ln -sf /dev/stdout /var/log/nginx/access.log \t&& ln -sf /dev/stderr /var/log/nginx/error.log"
        },
        {
            "created": "2019-06-11T00:03:10.074662734Z",
            "created_by": "/bin/sh -c #(nop)  EXPOSE 80",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:03:10.243466549Z",
            "created_by": "/bin/sh -c #(nop)  STOPSIGNAL SIGTERM",
            "empty_layer": true
        },
        {
            "created": "2019-06-11T00:03:10.403803169Z",
            "created_by": "/bin/sh -c #(nop)  CMD [\"nginx\" \"-g\" \"daemon off;\"]",
            "empty_layer": true
        }
    ]

From the last entry in the history we can see the `ngnix` binary is the image's entry point.


Examining Layers
===============

We can iterate over the layers given in an image starting with the manifest
file. The list of layers contained in the "Layers" attribute appear ordered
from bottom to top. The following are the layers contained in the nginx image.

    "Layers": [
      "a463b6f8359c7ecbbea7459ce4250c631057170989213e27d928b30f16d19b82/layer.tar",
      "531ffb108b7ab61937380949c17ad7cbcc4aa8956b35cdd6825df6de37a7bd74/layer.tar",
      "d4bb1266156923f59e1e53c9a15659975c57474e1ba90f1b4943e73d1c363f43/layer.tar"
    ]

The first layer in this image contains the filesystem of a Debian system, the
second layer contains nginx and its configuration, and the third layer contains
the `/var/log/nginx/` directory which redirects nginx's access and error logs
to stdout and stderr, respectively.

## Library dependencies for nginx

    wdblair@ubuntblue:~/ibm/dockertool/layer/usr/sbin$ readelf -d nginx

    Dynamic section at offset 0x121260 contains 31 entries:
      Tag        Type                          Name/Value
     0x0000000000000001 (NEEDED)             Shared library: [libdl.so.2]
     0x0000000000000001 (NEEDED)             Shared library: [libpthread.so.0]
     0x0000000000000001 (NEEDED)             Shared library: [libcrypt.so.1]
     0x0000000000000001 (NEEDED)             Shared library: [libpcre.so.3]
     0x0000000000000001 (NEEDED)             Shared library: [libssl.so.1.1]
     0x0000000000000001 (NEEDED)             Shared library: [libcrypto.so.1.1]
     0x0000000000000001 (NEEDED)             Shared library: [libz.so.1]
     0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
                                                                ^------------  Contained in the bottom layer

The DockerHub image of nginx also contains nginx-debug, which can be configured
to log debug information. I'm unsure this would cause any security concerns if
an adversary compromised the system (why would enabling logging help them if
they already control the system?).

## Files Relevant to the Entry Point

    wdblair@ubuntblue:~/ibm/dockertool/layer/usr/sbin$ strings nginx | grep "^/"
    /lib64/ld-linux-x86-64.so.2
    /* callbH <----
    /w+D          |
    /H9o          |
    /tEH          |
    /t3H          |--------- Not files
    /tiH          |
    /t;A          |
    /t{H          |
    /t-H <---------
    /var/run/nginx.pid
    /var/run/nginx.lock
    /etc/nginx/
    /etc/nginx/nginx.conf
    /var/log/nginx/error.log
    /dev/null
    /var/cache/nginx/client_temp
    /var/log/nginx/access.log
    /temp
    /index.html
    /var/cache/nginx/proxy_temp
    /var/cache/nginx/fastcgi_temp
    /var/cache/nginx/uwsgi_temp
    /var/cache/nginx/scgi_temp

## SysFlow for NGINX Hosting Static Binary 

|Evt #|T |Process                                      |PPID |PID  |TID  |Op Flags   |Start Time                |End Time                  |FD   |Ret  |Resource                                     |NOBRead |NOBWrite|Cont      |
|    0|PE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|EXEC       |06/12/2019T13:44:23.746047|                          |     |    0|                                             |:       |:       |0b703f366424|
|    1|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.746125|06/12/2019T13:44:23.746131|    3|     |/etc/ld.so.cache                             |0:0     |0:0     |0b703f366424|
|    2|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746141|06/12/2019T13:44:23.746164|    3|     |/lib/x86_64-linux-gnu/libdl.so.2             |1:832   |0:0     |0b703f366424|
|    3|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746175|06/12/2019T13:44:23.746194|    3|     |/lib/x86_64-linux-gnu/libpthread.so.0        |1:832   |0:0     |0b703f366424|
|    4|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746205|06/12/2019T13:44:23.746223|    3|     |/lib/x86_64-linux-gnu/libcrypt.so.1          |1:832   |0:0     |0b703f366424|
|    5|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746236|06/12/2019T13:44:23.746252|    3|     |/lib/x86_64-linux-gnu/libpcre.so.3           |1:832   |0:0     |0b703f366424|
|    6|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746268|06/12/2019T13:44:23.746285|    3|     |/usr/lib/x86_64-linux-gnu/libssl.so.1.1      |1:832   |0:0     |0b703f366424|
|    7|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746294|06/12/2019T13:44:23.746314|    3|     |/usr/lib/x86_64-linux-gnu/libcrypto.so.1.1   |1:832   |0:0     |0b703f366424|
|    8|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746331|06/12/2019T13:44:23.746349|    3|     |/lib/x86_64-linux-gnu/libz.so.1              |1:832   |0:0     |0b703f366424|
|    9|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.746367|06/12/2019T13:44:23.746386|    3|     |/lib/x86_64-linux-gnu/libc.so.6              |1:832   |0:0     |0b703f366424|
|   10|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.747148|06/12/2019T13:44:23.747163|    3|     |/etc/localtime                               |2:198   |0:0     |0b703f366424|
|   11|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749071|06/12/2019T13:44:23.749078|    4|     |/sys/devices/system/cpu/online               |1:4     |0:0     |0b703f366424|
|   12|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|  C     C  |06/12/2019T13:44:23.749181|06/12/2019T13:44:23.749183|    5|     |                                             |0:0     |0:0     |0b703f366424|
|   13|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|  C     C  |06/12/2019T13:44:23.749190|06/12/2019T13:44:23.749191|    5|     |                                             |0:0     |0:0     |0b703f366424|
|   14|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749202|06/12/2019T13:44:23.749213|    5|     |/etc/nsswitch.conf                           |2:497   |0:0     |0b703f366424|
|   15|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.749219|06/12/2019T13:44:23.749224|    5|     |/etc/ld.so.cache                             |0:0     |0:0     |0b703f366424|
|   16|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749236|06/12/2019T13:44:23.749255|    5|     |/lib/x86_64-linux-gnu/libnss_compat.so.2     |1:832   |0:0     |0b703f366424|
|   17|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749266|06/12/2019T13:44:23.749286|    5|     |/lib/x86_64-linux-gnu/libnsl.so.1            |1:832   |0:0     |0b703f366424|
|   18|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.749316|06/12/2019T13:44:23.749320|    5|     |/etc/ld.so.cache                             |0:0     |0:0     |0b703f366424|
|   19|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749332|06/12/2019T13:44:23.749349|    5|     |/lib/x86_64-linux-gnu/libnss_nis.so.2        |1:832   |0:0     |0b703f366424|
|   20|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749362|06/12/2019T13:44:23.749379|    5|     |/lib/x86_64-linux-gnu/libnss_files.so.2      |1:832   |0:0     |0b703f366424|
|   21|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.749403|06/12/2019T13:44:23.749422|    5|     |/etc/passwd                                  |0:0     |0:0     |0b703f366424|
|   22|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|  C     C  |06/12/2019T13:44:23.749433|06/12/2019T13:44:23.749434|    5|     |                                             |0:0     |0:0     |0b703f366424| <-- Ticket
|   23|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|  C     C  |06/12/2019T13:44:23.749440|06/12/2019T13:44:23.749441|    5|     |                                             |0:0     |0:0     |0b703f366424|
|   24|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.749448|06/12/2019T13:44:23.749464|    5|     |/etc/group                                   |0:0     |0:0     |0b703f366424|
|   25|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749573|06/12/2019T13:44:23.749684|    5|     |/etc/nginx/mime.types                        |2:5231  |0:0     |0b703f366424|
|   26|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.749700|06/12/2019T13:44:23.749715|    5|     |/etc/nginx/conf.d                            |0:0     |0:0     |0b703f366424|
|   27|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749721|06/12/2019T13:44:23.749750|    5|     |/etc/nginx/conf.d/default.conf               |1:1093  |0:0     |0b703f366424|
|   28|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O   R   C  |06/12/2019T13:44:23.749149|06/12/2019T13:44:23.749950|    4|     |/etc/nginx/nginx.conf                        |1:643   |0:0     |0b703f366424|
|   29|FE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|MKDIR      |06/12/2019T13:44:23.750402|                          |     |    0|/var/cache/nginx/client_temp                 |:       |:       |0b703f366424|
|   30|FE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|MKDIR      |06/12/2019T13:44:23.750446|                          |     |    0|/var/cache/nginx/proxy_temp                  |:       |:       |0b703f366424|
|   31|FE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|MKDIR      |06/12/2019T13:44:23.750469|                          |     |    0|/var/cache/nginx/fastcgi_temp                |:       |:       |0b703f366424|
|   32|FE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|MKDIR      |06/12/2019T13:44:23.750489|                          |     |    0|/var/cache/nginx/uwsgi_temp                  |:       |:       |0b703f366424|
|   33|FE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|MKDIR      |06/12/2019T13:44:23.750507|                          |     |    0|/var/cache/nginx/scgi_temp                   |:       |:       |0b703f366424|
|   34|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O  W    C  |06/12/2019T13:44:23.750600|06/12/2019T13:44:23.750610|    7|     |/var/run/nginx.pid                           |0:0     |1:2     |0b703f366424|
|   35|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O       C  |06/12/2019T13:44:23.747203|06/12/2019T13:44:23.750618|    3|     |/var/log/nginx/error.log                     |0:0     |0:0     |0b703f366424|
|   36|PE|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|CLONE      |06/12/2019T13:44:23.750697|                          |     |    6|                                             |:       |:       |0b703f366424|
|   37|PE|/usr/sbin/nginx                              | 6437| 6466| 6466|CLONE      |06/12/2019T13:44:23.750752|                          |     |    0|                                             |:       |:       |0b703f366424|
|   38|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O   R   C  |06/12/2019T13:44:23.750831|06/12/2019T13:44:23.750835|    8|     |/proc/sys/kernel/ngroups_max                 |1:6     |0:0     |0b703f366424|
|   39|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:23.750884|06/12/2019T13:44:23.750905|    8|     |/etc/group                                   |0:0     |0:0     |0b703f366424|
|   40|PE|/usr/sbin/nginx                              | 6437| 6466| 6466|SETUID     |06/12/2019T13:44:23.750912|                          |     |    0|                                             |:       |:       |0b703f366424|
|   41|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|        C  |06/12/2019T13:44:23.750953|06/12/2019T13:44:23.750953|   12|     |                                             |0:0     |0:0     |0b703f366424|
|   42|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|        C  |06/12/2019T13:44:23.750957|06/12/2019T13:44:23.750957|   11|     |                                             |0:0     |0:0     |0b703f366424|
|   43|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|        C  |06/12/2019T13:44:23.751091|06/12/2019T13:44:23.751091|    3|     |                                             |0:0     |0:0     |0b703f366424|
|   44|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:31.592055|06/12/2019T13:44:31.592120|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   45|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:33.235332|06/12/2019T13:44:33.235428|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   46|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:34.256302|06/12/2019T13:44:34.256375|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   47|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:35.248872|06/12/2019T13:44:35.248960|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   48|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:36.205827|06/12/2019T13:44:36.205873|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   49|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:37.118454|06/12/2019T13:44:37.118504|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   50|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:38.066808|06/12/2019T13:44:38.066853|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   51|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:39.082294|06/12/2019T13:44:39.082337|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   52|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:40.080052|06/12/2019T13:44:40.080095|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   53|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|O       C  |06/12/2019T13:44:41.076482|06/12/2019T13:44:41.076525|   11|     |/usr/share/nginx/html/index.html             |0:0     |0:0     |0b703f366424|
|   54|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O        T |06/12/2019T13:44:23.750522|06/12/2019T13:44:48.592115|    4|     |/var/log/nginx/error.log                     |0:0     |0:0     |0b703f366424|
|   55|FF|/usr/sbin/nginx -g daemon off;               |     | 6437| 6437|O        T |06/12/2019T13:44:23.750531|06/12/2019T13:44:48.592139|    5|     |/var/log/nginx/access.log                    |0:0     |0:0     |0b703f366424|
16777343
|   56|NF|/usr/sbin/nginx                              | 6437| 6466| 6466| A WR    T |06/12/2019T13:44:29.939481|06/12/2019T13:44:48.594687|    3|     |127.0.0.1:53362-127.0.0.1:80                 |10:4214 |10:1800 |0b703f366424|
|   57|FF|/usr/sbin/nginx                              | 6437| 6466| 6466|   W     T |06/12/2019T13:44:31.592118|06/12/2019T13:44:48.594703|    5|     |/var/log/nginx/access.log                    |0:0     |10:1530 |0b703f366424|

## Files used in the trace that were not given above:

  The following files appeared in the sysflow trace for nginx, but not in my
  initial static model of the nginx image. A likely explanation of the
  discrepency is that the files given above are only contained in the nginx
  binary. Recursively obtaining files given in binaries and configuration files
  may lead to the full set that we see in the trace.

  /etc/ld.so.cache
  /etc/localtime
  /sys/devices/system/cpu/online
  /etc/nsswitch.conf
  /lib/x86_64-linux-gnu/libnsl.so.1
  /lib/x86_64-linux-gnu/libnss_compat.so.2
  /lib/x86_64-linux-gnu/libnss_nis.so.2
  /lib/x86_64-linux-gnu/libnss_files.so.2
  /etc/passwd
  /etc/group
  /etc/nginx/mime.types
  /etc/nginx/conf.d
  /etc/nginx/conf.d/default.conf
  /etc/nginx/nginx.conf
  /usr/share/nginx/html/index.html <-- nginx recommends placing static content in its own docker layer by building on the nginx container.
  /proc/sys/kernel/ngroups_max

## Designing a Defense 

One defense could restrict reads to only the files reachable from the entry
point, and writes to only those files marked as writeable in the file system.
