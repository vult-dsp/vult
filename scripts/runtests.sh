#!/bin/bash
./configure --enable-tests
make
_build/src/test/top.native -testdata-dir `pwd`/src/test/