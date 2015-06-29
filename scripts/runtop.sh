#!/bin/bash
./scripts/build.sh
utop -init toplevel.init -I _build/src -I _build/src/util -I _build/src/core -I _build/src/parser -I _build/src/passes
