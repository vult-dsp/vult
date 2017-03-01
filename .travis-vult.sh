sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install containers ppx_deriving ounit js_of_ocaml pla ollvm
make all
cp ./_build/src/vultc.native ./vultc
if [ $TRAVIS_OS_NAME == linux ];
then
   tar -cvzf vult-linux.tar.gz vultc runtime/vultin.h runtime/vultin.c
else
   tar -cvzf vult-osx.tar.gz vultc runtime/vultin.h runtime/vultin.c
fi
npm install vult -g
vultc --help
