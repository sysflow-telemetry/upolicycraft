#!/usr/bin/env bash

ls *.o | xargs -L1 ./lowerswitch.sh 
