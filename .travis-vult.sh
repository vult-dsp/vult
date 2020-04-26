sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)
# installs all dependencies
opam install containers ppx_deriving ounit js_of_ocaml js_of_ocaml-ppx pla ocveralls bisect_ppx result
# run the coverage
#make coverage
#make clean
# builds the release version
make all
# prepare to package
cp _build/default/src/vult.exe  ./vult
if [ $TRAVIS_OS_NAME == linux ];
then
   tar -cvzf vult-linux.tar.gz vult runtime/vultin.h runtime/vultin.cpp
else
   tar -cvzf vult-osx.tar.gz vult runtime/vultin.h runtime/vultin.cpp
fi
# test the instalation of vult from npm and check it runs
#npm install vult -g
#vult --help
# compile the examples
#cd examples
#mkdir build
#cd build
#cmake ../
#make
#cd ..
#cd ..
# runs the performance tests
#make perf
