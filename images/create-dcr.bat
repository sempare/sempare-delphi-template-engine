@echo off

echo Run this from the images directory
pause
cd ..\src
 cgrc -c65001 TSempareBootVelocityContext.rc -foTSempareBootVelocityContext.dcr
 cgrc -c65001 TSempareBootVelocityTemplate.rc -foTSempareBootVelocityTemplate.dcr
 cgrc -c65001 TSempareBootVelocityEngine.rc -foTSempareBootVelocityEngine.dcr
pause
