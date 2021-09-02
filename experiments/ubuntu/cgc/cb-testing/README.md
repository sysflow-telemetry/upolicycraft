Reproducing CGC Evaluation:

In order to model an individual challenge, I would recommend using the
`cb-replay-dump` utility which allows you to generate a binary data file
directly from a CGC XML file (normally located in the poller/for-release
folder). You can then pass that file directly to `uIDS` through `stdin` instead
of having to prop up `uIDS` behind an xinetd server.

The output will go in `/tmp/data`.
