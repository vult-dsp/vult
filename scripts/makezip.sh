cd $APPVEYOR_BUILD_FOLDER
7z a $APPVEYOR_BUILD_FOLDER/vult-win.zip $APPVEYOR_BUILD_FOLDER/vultc.exe $APPVEYOR_BUILD_FOLDER/runtime/*.*
npm install vult -g
vultc --help