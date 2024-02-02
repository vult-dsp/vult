PREFIX ?= /usr/local/bin

VULT_SRC = $(wildcard src/*.ml) $(wildcard src/core/*.ml) $(wildcard src/generators/*.ml) $(wildcard src/js/*.ml) $(wildcard src/parser/*.ml) $(wildcard src/passes/*.ml) $(wildcard src/util/*.ml) $(wildcard test/*.ml)

OCB = ocamlbuild -j 4 -use-ocamlfind

ifeq (, $(shell which ocamlformat))
	FORMAT =
else
	FORMAT = @fmt --auto-promote
endif

compiler: version
	dune build src/vult.bc src/vult.exe $(FORMAT)

run: compiler
	./_build/default/src/vult.exe

#js: jscompiler
#	$(OCB) src/js/vultlib.byte
#	js_of_ocaml vultlib.byte

jscompiler:
	dune build src/vultjs.bc $(FORMAT)
	js_of_ocaml --custom-header="#!/usr/bin/env node" --disable use-js-string _build/default/src/vultjs.bc -o vult.js
	chmod +x vult.js

#web:
#	$(OCB) src/js/vultweb.byte
#	js_of_ocaml vultweb.byte
#	sed -i -e "s/this.fs=require(..)/this.fs=null/g" vultweb.js

test: jscompiler
	dune build test/test.exe src/vult.bc src/vult.exe $(FORMAT)
	./_build/default/test/test.exe

perf:
	dune build test/perf.exe $(FORMAT)
	./_build/default/test/perf.exe

test-fast:
	dune build test/test.exe src/vult.bc src/vult.exe $(FORMAT)
	./_build/default/test/test.exe -internal true

test-update:
	dune build test/test.exe src/vult.bc src/vult.exe $(FORMAT)
	./_build/default/test/test.exe -update true -internal true

coverage:
	dune build test/test.exe src/vult.bc src/vult.exe $(FORMAT) --instrument-with bisect_ppx --force
	./_build/default/test/test.exe -internal true
	bisect-ppx-report html
	find . -name '*.coverage' | xargs rm -f
	open _coverage/index.html
#	bisect-ppx-report send-to Coveralls --source-path _build

VERSION:=$(shell git describe --tags --abbrev=0)

version :
	@echo "let version = String.trim \"" $(VERSION) "\"" > src/core/version.ml

all: version compiler #js test web jscompiler

clean:
	dune clean
	rm -f vult.js vultweb.js vultlib.js
	rm -f bisect*.out
	rm -rf bisect_coverage
	rm -rf _build

install:
	cp _build/default/src/vult.exe $(PREFIX)/vult

install-lib:
	dune build -p vult @install

.PHONY: 	all clean compiler js test