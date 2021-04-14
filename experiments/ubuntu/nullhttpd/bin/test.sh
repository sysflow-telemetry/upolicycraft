#!/usr/bin/env bash

curl http://localhost:2051/
curl http://localhost:2051/docs/static_file.html

# Trigger the Cache 
curl http://localhost:2051/docs/static_file.html
curl -v --user wdblair:password http://localhost:2051/recv
curl 'http://localhost:2051/foobar/?passwd=foobar&uids_foo=wicked&uids_content=waycomes&uids_directory=root&passwd=baz'
curl http://localhost:2051/index.html
curl http://localhost:2051/docs/old/test.html
curl http://localhost:2051/docs/missing

# Trigger sanitisers
curl http://localhost:2051/../../../..///../../../etc/passwd
curl http://localhost:2051/nonexistant.html
curl http://localhost:2051/giphy.gif

curl -d '{"foo": "bar"}' -H 'Content-Type: application/json' -X POST http://localhost:2051/recv

curl -X PUT http://localhost:2051/
