cd $APPVEYOR_BUILD_FOLDER
ocaml setup.ml -build
ocaml setup.ml -install
7z a $APPVEYOR_BUILD_FOLDER/vult.zip `opam config var bin`/vultc $APPVEYOR_BUILD_FOLDER/runtime/vultin.c $APPVEYOR_BUILD_FOLDER/runtime/vultin.h