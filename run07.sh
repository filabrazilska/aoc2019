#!/bin/bash

pushd ebin && erlc ../src/intcode.erl; popd
./day07.escript input07.txt    | grep -B 1 '^>>' | grep -v '^>>' | sort -n
