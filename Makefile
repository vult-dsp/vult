OCB = ocamlbuild -j 4 -use-ocamlfind

compiler: version
			$(OCB) src/vultc.native src/vultc.byte

js:
			$(OCB) src/js/vultjs.byte
			$(OCB) src/js/vultweb.byte
			$(OCB) src/js/vultlib.byte
			js_of_ocaml vultjs.byte
			js_of_ocaml vultweb.byte
			js_of_ocaml vultlib.byte

test:
			$(OCB) test/test.native
			./test.native -runner sequential

version :
			@echo "let version = \"" > src/version.ml
			@git describe --abbrev=0 >> src/version.ml
			@echo "\"" >> src/version.ml

test-update:
			$(OCB) test/test.native
			./test.native -runner sequential -writeout true

all: 		compiler js test

clean:
			$(OCB) -clean
			rm -f vultjs.js vultweb.js vultlib.js

.PHONY: 	all clean compiler js test