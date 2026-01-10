@ECHO OFF
CD ..\..\libpvn\src
nmake.exe /NOLOGO CRT=Md NDEBUG=3 clean all
CD ..\..\KogAcc\src
nmake.exe clean all
CD ..\etc
