..\bin\$(PLAT)\sksvd2.exe: test\sksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\sksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\cksvd2.exe: test\cksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\cksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\dksvd2.exe: test\dksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\dksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\zksvd2.exe: test\zksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\zksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\qksvd2.exe: test\qksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\qksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\yksvd2.exe: test\yksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\yksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\slwsv2.exe: test\slwsv2x.F90 test\tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\slwsv2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\dlwsv2.exe: test\dlwsv2x.F90 test\tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) /fpp test\dlwsv2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\spqsort.exe: test\spqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) test\spqsortx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\dpqsort.exe: test\dpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) test\dpqsortx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\qpqsort.exe: test\qpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) test\qpqsortx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\srnd.exe: test\srnd.f90 $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) test\srnd.f90 /Fe$@ $(LDFLAGS)

..\bin\$(PLAT)\sthalt.exe: test\sthaltx.f90 ..\lib\$(PLAT)\faux.lib $(MKFS) test\Makefile
	$(FC) $(FCFLAGS) test\sthaltx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\faux.lib
