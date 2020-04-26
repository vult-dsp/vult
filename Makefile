PREFIX ?= /usr/local/bin

VULT_SRC = $(wildcard src/*.ml) $(wildcard src/core/*.ml) $(wildcard src/generators/*.ml) $(wildcard src/js/*.ml) $(wildcard src/parser/*.ml) $(wildcard src/passes/*.ml) $(wildcard src/util/*.ml) $(wildcard test/*.ml)

OCB = ocamlbuild -j 4 -use-ocamlfind

ifeq (, $(shell which ocamlformat))
	FORMAT =
else
	FORMAT = @fmt --auto-promote
endif

compiler:
	dune build src/vult.bc src/vult.exe $(FORMAT)

run: compiler
	./_build/default/src/vult.exe

#js: jscompiler
#	$(OCB) src/js/vultlib.byte
#	js_of_ocaml vultlib.byte

jscompiler:
	dune build src/vultjs.bc $(FORMAT)
	js_of_ocaml --custom-header="#!/usr/bin/env node" _build/default/src/vultjs.bc -o vultc.js
	chmod +x vultc.js

#web:
#	$(OCB) src/js/vultweb.byte
#	js_of_ocaml vultweb.byte
#	sed -i -e "s/this.fs=require(..)/this.fs=null/g" vultweb.js

#test: compiler jscompiler
#	$(OCB) test/test.native
#	./test.native -runner sequential -shards 1

#perf:
#	$(OCB) test/perf.native
#	./perf.native

#test-fast:
#	$(OCB) test/test.native
#	./test.native -runner sequential -shards 1 -internal true

#test-update:
#	$(OCB) test/test.native
#	./test.native -runner sequential -update true -shards 1 -internal true

#coverage: compiler jscompiler
#	$(OCB) -clean
#	BISECT_COVERAGE=YES $(OCB) test/test.native
#	BISECT_COVERAGE=YES $(OCB) src/vultc.native
#	BISECT_COVERAGE=YES $(OCB) test/perf.native
#	BISECT_FILE=_build/coverage ./test.native -runner sequential -shards 1
#	BISECT_FILE=_build/coverage ./perf.native
#	bisect-ppx-report send-to Coveralls --source-path _build

VERSION:=$(shell git describe --tags --abbrev=0)

version :
	@echo "let version = String.trim \"" $(VERSION) "\"" > src/driver/version.ml

all: version compiler #js test web jscompiler

clean:
	dune clean
	rm -f vultc.js vultweb.js vultlib.js
	rm -f bisect*.out
	rm -rf bisect_coverage

install:
	cp _build/default/src/vult.exe $(PREFIX)/vult

.PHONY: 	all clean compiler js test