set PATH=%PATH%;C:\windows\Microsoft.NET\Framework\v4.0.30319
set PATH=%PATH%;C:\Program Files (x86)\Embarcadero\Studio\23.0\bin
call rsvars.bat
cd ..
msbuild Sempare.Template.Tester.dproj /t:Clean /p:Config=%~1 /p:Platform=%~2
msbuild Sempare.Template.Tester.dproj /t:Build /p:Config=%~1 /p:Platform=%~2
