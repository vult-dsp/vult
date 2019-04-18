#!/usr/bin/env sh

# removes the cygwin link because has the same name as the Visual C++ link
rm /usr/bin/link.exe

wget https://github.com/fdopen/opam-repository-mingw/releases/download/0.0.0.2/opam64.tar.xz
tar -xf 'opam64.tar.xz'
./opam64/install.sh

opam init default "https://github.com/fdopen/opam-repository-mingw.git#opam2" -c "ocaml-variants.4.07.1+mingw64" --disable-sandboxing
eval $(ocaml-env cygwin)

opam install pla containers ppx_deriving ounit yojson menhir result
# build vult
cd $APPVEYOR_BUILD_FOLDER
ls
pwd
make
# install vultc from npm
npm install -g vult
