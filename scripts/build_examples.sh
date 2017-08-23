cd $APPVEYOR_BUILD_FOLDER
cd examples
mkdir build
cd build
cmake ../ -G "NMake Makefiles"
nmake