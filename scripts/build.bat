call /b rsvars.bat
msbuild Sempare.Template.Tester.dproj /t:Clean /p:Config=Debug /p:Platform=Win32
msbuild Sempare.Template.Tester.dproj /t:Build /p:Config=Debug /p:Platform=Win32
