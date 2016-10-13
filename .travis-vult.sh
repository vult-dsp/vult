sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install containers ppx_deriving ounit js_of_ocaml pla
./configure --enable-tests --enable-js
make test
cp ./_build/src/vultc.native ./vultc
if [ $TRAVIS_OS_NAME == linux ];
then
   tar -cvzf vult-v0.3-linux.tar.gz vultc runtime/vultin.h runtime/vultin.c
else
   tar -cvzf vult-v0.3-osx.tar.gz vultc runtime/vultin.h runtime/vultin.c
fi