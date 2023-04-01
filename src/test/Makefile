..\bin\$(PLAT)\sksvd2.exe: test\sksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\sksvd2x.F90 /Fesksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\cksvd2.exe: test\cksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\cksvd2x.F90 /Fecksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\dksvd2.exe: test\dksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\dksvd2x.F90 /Fedksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\zksvd2.exe: test\zksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\zksvd2x.F90 /Fezksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\qksvd2.exe: test\qksvd2x.F90 test\tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\qksvd2x.F90 /Feqksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\yksvd2.exe: test\yksvd2x.F90 test\uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\yksvd2x.F90 /Feyksvd2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\slwsv2.exe: test\slwsv2x.F90 test\tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\slwsv2x.F90 /Feslwsv2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\dlwsv2.exe: test\dlwsv2x.F90 test\tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\dlwsv2x.F90 /Fedlwsv2.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\spqsort.exe: test\spqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\spqsortx.f90 /Fespqsort.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\dpqsort.exe: test\dpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\dpqsortx.f90 /Fedpqsort.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\qpqsort.exe: test\qpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\qpqsortx.f90 /Feqpqsort.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\srnd.exe: test\srnd.f90 test\grnd.f90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\srnd.f90 /Fesrnd.exe $(LDFLAGS)

..\bin\$(PLAT)\crnd.exe: test\crnd.f90 test\hrnd.f90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\crnd.f90 /Fecrnd.exe $(LDFLAGS)

..\bin\$(PLAT)\drnd.exe: test\drnd.f90 test\grnd.f90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\drnd.f90 /Fedrnd.exe $(LDFLAGS)

..\bin\$(PLAT)\zrnd.exe: test\zrnd.f90 test\hrnd.f90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\zrnd.f90 /Fezrnd.exe $(LDFLAGS)

..\bin\$(PLAT)\qrnd.exe: test\qrnd.f90 test\grnd.f90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\qrnd.f90 /Feqrnd.exe $(LDFLAGS)

..\bin\$(PLAT)\yrnd.exe: test\yrnd.f90 test\hrnd.f90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\yrnd.f90 /Feyrnd.exe $(LDFLAGS)

..\bin\$(PLAT)\srnd2u.exe: test\srnd2t.F90 test\grnd2t.F90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /DUPPER=.TRUE. /fpp ..\..\..\src\test\srnd2t.F90 /Fesrnd2u.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\faux.lib $(LAPACK)

..\bin\$(PLAT)\srnd2g.exe: test\srnd2t.F90 test\grnd2t.F90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\srnd2t.F90 /Fesrnd2g.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\faux.lib

..\bin\$(PLAT)\crnd2g.exe: test\crnd2t.F90 test\hrnd2t.F90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\crnd2t.F90 /Fecrnd2g.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\faux.lib

..\bin\$(PLAT)\drnd2u.exe: test\drnd2t.F90 test\grnd2t.F90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /DUPPER=.TRUE. /fpp ..\..\..\src\test\drnd2t.F90 /Fedrnd2u.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\faux.lib $(LAPACK)

..\bin\$(PLAT)\drnd2g.exe: test\drnd2t.F90 test\grnd2t.F90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\drnd2t.F90 /Fedrnd2g.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\faux.lib

..\bin\$(PLAT)\qrnd2g.exe: test\qrnd2t.F90 test\grnd2t.F90 $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\qrnd2t.F90 /Feqrnd2g.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\faux.lib

..\bin\$(PLAT)\sthalt.exe: test\sthaltx.f90 ..\lib\$(PLAT)\faux.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\sthaltx.f90 /Festhalt.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\faux.lib

..\bin\$(PLAT)\jsweep.exe: test\jsweepx.f90 ..\lib\$(PLAT)\faux.lib $(MKFS) test\Makefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) ..\..\..\src\test\jsweepx.f90 /Fejsweep.exe $(LDFLAGS) ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib

..\bin\$(PLAT)\sksvd0.exe: test\sksvd0x.F90 test\gksvd0x.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\sksvd0x.F90 /Fesksvd0.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib $(LAPACK)

..\bin\$(PLAT)\cksvd0.exe: test\cksvd0x.F90 test\hksvd0x.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\cksvd0x.F90 /Fecksvd0.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib $(LAPACK)

..\bin\$(PLAT)\dksvd0.exe: test\dksvd0x.F90 test\gksvd0x.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\dksvd0x.F90 /Fedksvd0.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib $(LAPACK)

..\bin\$(PLAT)\zksvd0.exe: test\zksvd0x.F90 test\hksvd0x.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\zksvd0x.F90 /Fezksvd0.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib $(LAPACK)

..\bin\$(PLAT)\qksvd0.exe: test\qksvd0x.F90 test\gksvd0x.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\qksvd0x.F90 /Feqksvd0.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib

..\bin\$(PLAT)\yksvd0.exe: test\yksvd0x.F90 test\hksvd0x.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\yksvd0x.F90 /Feyksvd0.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib ..\..\..\..\JACSD\jstrat$(DEBUG).lib

..\bin\$(PLAT)\sksvdd.exe: test\sksvddx.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\sksvddx.F90 /Fesksvdd.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib $(LAPACK)

..\bin\$(PLAT)\cksvdd.exe: test\cksvddx.F90 ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\faux.lib $(MKFS) test/GNUmakefile
	cd ..\bin\$(PLAT) && $(FC) $(FCFLAGS) /fpp ..\..\..\src\test\cksvddx.F90 /Fecksvdd.exe ..\..\..\lib\$(PLAT)\blksvd.lib ..\..\..\lib\$(PLAT)\dynord.lib ..\..\..\lib\$(PLAT)\ksvd2.lib ..\..\..\lib\$(PLAT)\arrbio.lib ..\..\..\lib\$(PLAT)\faux.lib $(LAPACK)
