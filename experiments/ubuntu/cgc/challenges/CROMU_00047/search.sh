#!/usr/bin/env bash

for f in $(ls poller/for-release/inputs/*.dat); do ./CROMU_00047.sh < $f; printf "Attempting %s\n" $f; ./report.sh report CROMU_00047; done | tee coverage-data.log
