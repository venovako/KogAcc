@ECHO OFF
FOR /L %%I IN (128,128,5376) DO ..\..\..\JACSD\tgensvd\dgensvd.exe %%I.txt 1 %%I %%I d%%I
