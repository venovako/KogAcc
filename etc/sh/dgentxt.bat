@ECHO OFF
FOR /L %%I IN (128,128,5376) DO ..\..\..\JACSD\tgensvd\dgentxt.exe %%I -23 0 > %%I.txt
