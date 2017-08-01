OCB = ocamlbuild -j 4 -use-ocamlfind

compiler: version
			$(OCB) src/vultc.native src/vultc.byte

js: jscompiler
			$(OCB) src/js/vultlib.byte
			js_of_ocaml --opt 3 vultlib.byte

jscompiler: compiler
			js_of_ocaml --opt 3 vultc.byte

web:
			$(OCB) src/js/vultweb.byte
			js_of_ocaml --opt 3 vultweb.byte
			sed -i -e "s/require(/foo(/g" vultweb.js

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
			$(OCB) -clean
			BISECT_COVERAGE=YES $(OCB) test/test.native
			./test.native -runner sequential -shards 1 -coverage true
			bisect-ppx-report -I _build/ -html bisect_coverage/ bisect*.out

all: 		compiler js test web jscompiler

clean:
			$(OCB) -clean
			rm -f vultc.js vultweb.js vultlib.js
			rm -f bisect*.out
			rm -rf bisect_coverage

.PHONY: 	all clean compiler js test