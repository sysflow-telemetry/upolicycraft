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

We could imagine one defense would restrict reads to only the files given here,
library dependencies, and any volumes attached to the container.
