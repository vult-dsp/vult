#!/bin/bash
cat ./scripts/debugscript.base > ./scripts/debugscript
echo directory `ocamlfind query containers` >> ./scripts/debugscript
