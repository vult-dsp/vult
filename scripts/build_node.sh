ocamlbuild -use-ocamlfind -pkg containers vult_node.byte -syntax camlp4o -pkg js_of_ocaml -pkg js_of_ocaml.syntax
js_of_ocaml vult_node.byte
cp vult_node.js ./node/public/javascripts/
