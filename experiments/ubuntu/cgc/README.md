CGC Evaluation:

Challenge sources are given in the `challenges` directory, with input located in
the `poller/for-release` directory in the challenge. If input isn't present, you can
generate the inputs using the `generate-polls` utility in the `poll-generator` folder.

Often challenges require complex binary inputs, and I found an easier way to use these
inputs for modeling is to dump the inputs as binary data by using the `cb-replay-dump`
utility found in the `cb-testing` directory.