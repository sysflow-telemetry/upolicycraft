#!/usr/bin/env bash

find . -name *.o -exec ./lowerswitch.sh {} \;
