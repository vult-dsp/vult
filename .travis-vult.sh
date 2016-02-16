sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install containers ppx_deriving ounit
./configure --enable-tests
make test