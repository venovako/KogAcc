..\bin\$(PLAT)\sksvd2.exe: test\sksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\sksvd2x.F90 /Fesksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib && popd

..\bin\$(PLAT)\cksvd2.exe: test\cksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\cksvd2x.F90 /Fecksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib && popd

..\bin\$(PLAT)\dksvd2.exe: test\dksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\dksvd2x.F90 /Fedksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib && popd

..\bin\$(PLAT)\zksvd2.exe: test\zksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\zksvd2x.F90 /Fezksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib && popd

..\bin\$(PLAT)\qksvd2.exe: test\qksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\qksvd2x.F90 /Feqksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib && popd

..\bin\$(PLAT)\yksvd2.exe: test\yksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\yksvd2x.F90 /Feyksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib && popd

..\bin\$(PLAT)\slwsv2.exe: test\slwsv2x.F90 test\tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\slwsv2x.F90 /Feslwsv2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib $(LAPACK) && popd

..\bin\$(PLAT)\dlwsv2.exe: test\dlwsv2x.F90 test\tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\dlwsv2x.F90 /Fedlwsv2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib $(LAPACK) && popd

..\bin\$(PLAT)\spqsort.exe: test\spqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\spqsortx.f90 /Fespqsort.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\dynord.lib && popd

..\bin\$(PLAT)\dpqsort.exe: test\dpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\dpqsortx.f90 /Fedpqsort.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\dynord.lib && popd

..\bin\$(PLAT)\qpqsort.exe: test\qpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\qpqsortx.f90 /Feqpqsort.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\dynord.lib && popd

..\bin\$(PLAT)\srnd.exe: test\srnd.f90 $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\srnd.f90 /Fesrnd.exe $(LDFLAGS) && popd

..\bin\$(PLAT)\sthalt.exe: test\sthaltx.f90 ..\lib\$(PLAT)\faux.lib $(MKFS) test\Makefile
	pushd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\sthaltx.f90 /Festhalt.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\faux.lib && popd
