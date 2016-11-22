cd $APPVEYOR_BUILD_FOLDER
cp $APPVEYOR_BUILD_FOLDER/_build/src/vultc.native $APPVEYOR_BUILD_FOLDER/vultc.exe
7z a $APPVEYOR_BUILD_FOLDER/vult-win.zip $APPVEYOR_BUILD_FOLDER/vultc.exe $APPVEYOR_BUILD_FOLDER/runtime/*.*
7z a $APPVEYOR_BUILD_FOLDER/vult-dll.zip $APPVEYOR_BUILD_FOLDER/build/shared/vult.dll $APPVEYOR_BUILD_FOLDER/build/shared/vult.lib $APPVEYOR_BUILD_FOLDER/build/shared/vult.exp
npm install vult -g
vultc --help