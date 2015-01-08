#!/bin/bash
cat debugscript.base > debugscript
echo directory `ocamlfind query containers` >> debugscript
