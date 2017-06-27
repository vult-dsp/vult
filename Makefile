OCB = ocamlbuild -j 4 -use-ocamlfind

compiler: version
			$(OCB) src/vultc.native src/vultc.byte

js: jscompiler
			$(OCB) src/js/vultweb.byte
			$(OCB) src/js/vultlib.byte
			js_of_ocaml vultweb.byte
			js_of_ocaml vultlib.byte

jscompiler:
			$(OCB) src/js/vultjs.byte
			js_of_ocaml vultjs.byte

test: compiler jscompiler
			$(OCB) test/test.native
			./test.native -runner sequential -shards 1

version :
			@echo "let version = \"" > src/version.ml
			@git describe --abbrev=0 >> src/version.ml
			@echo "\"" >> src/version.ml

test-update: compiler jscompiler
			$(OCB) test/test.native
			./test.native -runner sequential -update true -shards 1

coverage: compiler
			BISECT_COVERAGE=YES $(OCB) test/test.native
			./test.native -runner sequential -shards 1 -coverage true
			bisect-ppx-report -I _build/ -html bisect_coverage/ bisect*.out

all: 		compiler js test

clean:
			$(OCB) -clean
			rm -f vultjs.js vultweb.js vultlib.js
			rm -f bisect*.out
			rm -rf bisect_coverage

.PHONY: 	all clean compiler js test