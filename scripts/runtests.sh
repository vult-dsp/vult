#!/bin/bash
./configure --enable-tests
make
_build/test/top.native -testdata-dir `pwd`/test/