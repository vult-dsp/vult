OCB = ocamlbuild -use-ocamlfind

compiler:
			$(OCB) src/vultc.native src/vultc.byte

js:
			$(OCB) src/vultjs.byte
			$(OCB) src/vult_node.byte
			js_of_ocaml vultjs.byte
			js_of_ocaml vult_node.byte

test:
			$(OCB) test/test.native
			./test.native -runner sequential

test-update:
			$(OCB) test/test.native
			./test.native -runner sequential -writeout true

all: 		compiler js test

clean:
			$(OCB) -clean
			rm -f vultjs.js vult_node.js

.PHONY: 	all clean compiler js test