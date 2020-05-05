@echo off
del *.res
ren lazres.cmd lazres.exe
echo Create BZScene Tools Resources
lazres.exe BZScene_tools.res @bzscene_tools.rc
echo Create BZScene Audio Resources
lazres.exe bzscene_audio.res @bzscene_audio.rc
echo Create BZScene Controls Resources
lazres.exe bzscene_controls.res @bzscene_controls.rc
ren lazres.exe lazres.cmd
pause