sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install containers ppx_deriving ounit js_of_ocaml
./configure --enable-tests --enable-js
make test