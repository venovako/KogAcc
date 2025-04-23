@ECHO OFF
FOR /L %%I IN (128,128,5376) DO ..\..\..\JACSD\tgensvd\dgesvj_test.exe %%I %%I d%%I
