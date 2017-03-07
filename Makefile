OCB = ocamlbuild -use-ocamlfind

compiler: version
			$(OCB) src/vultc.native src/vultc.byte

js:
			$(OCB) src/vultjs.byte
			$(OCB) src/vultweb.byte
			js_of_ocaml vultjs.byte
			js_of_ocaml vultweb.byte

test:
			$(OCB) test/test.native
			./test.native -runner sequential

version :
			@echo "let version = \"" > src/version.ml
			@git describe >> src/version.ml
			@echo "\"" >> src/version.ml

test-update:
			$(OCB) test/test.native
			./test.native -runner sequential -writeout true

all: 		compiler js test

clean:
			$(OCB) -clean
			rm -f vultjs.js vultweb.js

.PHONY: 	all clean compiler js test