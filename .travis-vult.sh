sh .travis-ocaml.sh
export OPAMYES=1
opam switch 4.04.2 -y
eval $(opam config env)
# installs all dependencies
opam install containers ppx_deriving ounit js_of_ocaml js_of_ocaml-ppx pla ollvm ocveralls bisect_ppx
# run the coverage
make coverage
make clean
# builds the release version
make all
# prepare to package
cp ./_build/src/vultc.native ./vultc
if [ $TRAVIS_OS_NAME == linux ];
then
   tar -cvzf vult-linux.tar.gz vultc runtime/vultin.h runtime/vultin.c
else
   tar -cvzf vult-osx.tar.gz vultc runtime/vultin.h runtime/vultin.c
fi
# test the instalation of vulc from npm and check it runs
npm install vult -g
vultc --help
# compile the examples
cd examples
mkdir build
cd build
cmake ../
make
cd ..
cd ..
# runs the performance tests
make perf
