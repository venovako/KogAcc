../bin/$(PLAT)/sksvd2.exe: test/sksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o sksvd2.exe ../../../src/test/sksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux

../bin/$(PLAT)/cksvd2.exe: test/cksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o cksvd2.exe ../../../src/test/cksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux

../bin/$(PLAT)/dksvd2.exe: test/dksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dksvd2.exe ../../../src/test/dksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux

../bin/$(PLAT)/zksvd2.exe: test/zksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o zksvd2.exe ../../../src/test/zksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux

../bin/$(PLAT)/qksvd2.exe: test/qksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qksvd2.exe ../../../src/test/qksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/yksvd2.exe: test/yksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yksvd2.exe ../../../src/test/yksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/xksvd2.exe: test/xksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xksvd2.exe ../../../src/test/xksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/wksvd2.exe: test/wksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wksvd2.exe ../../../src/test/wksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/slwsv2.exe: test/slwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o slwsv2.exe ../../../src/test/slwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/dlwsv2.exe: test/dlwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dlwsv2.exe ../../../src/test/dlwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/spqsort.exe: test/spqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o spqsort.exe ../../../src/test/spqsortx.f90 -L../../../lib/$(PLAT) -ldynord

../bin/$(PLAT)/dpqsort.exe: test/dpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dpqsort.exe ../../../src/test/dpqsortx.f90 -L../../../lib/$(PLAT) -ldynord

../bin/$(PLAT)/qpqsort.exe: test/qpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qpqsort.exe ../../../src/test/qpqsortx.f90 -L../../../lib/$(PLAT) -ldynord

../bin/$(PLAT)/xpqsort.exe: test/xpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xpqsort.exe ../../../src/test/xpqsortx.f90 -L../../../lib/$(PLAT) -ldynord

../bin/$(PLAT)/srnd.exe: test/srnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o srnd.exe ../../../src/test/srnd.f90

../bin/$(PLAT)/crnd.exe: test/crnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o crnd.exe ../../../src/test/crnd.f90

../bin/$(PLAT)/drnd.exe: test/drnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o drnd.exe ../../../src/test/drnd.f90

../bin/$(PLAT)/zrnd.exe: test/zrnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o zrnd.exe ../../../src/test/zrnd.f90

../bin/$(PLAT)/qrnd.exe: test/qrnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qrnd.exe ../../../src/test/qrnd.f90

../bin/$(PLAT)/yrnd.exe: test/yrnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yrnd.exe ../../../src/test/yrnd.f90

../bin/$(PLAT)/xrnd.exe: test/xrnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xrnd.exe ../../../src/test/xrnd.f90

../bin/$(PLAT)/wrnd.exe: test/wrnd.f90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wrnd.exe ../../../src/test/wrnd.f90

../bin/$(PLAT)/sthalt.exe: test/sthaltx.f90 ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o sthalt.exe ../../../src/test/sthaltx.f90 -L../../../lib/$(PLAT) -lfaux

../bin/$(PLAT)/jsweep.exe: test/jsweepx.f90 ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o jsweep.exe ../../../src/test/jsweepx.f90 -L../../../lib/$(PLAT) -lfaux -L../../../../JACSD -ljstrat$(DEBUG)

../bin/$(PLAT)/s2g101.exe: test/s2g101.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o s2g101.exe ../../../src/test/s2g101.F90 -L../../../lib/$(PLAT) -lfaux

../bin/$(PLAT)/s2g110.exe: test/s2g110.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o s2g110.exe ../../../src/test/s2g110.F90 -L../../../lib/$(PLAT) -lfaux

../bin/$(PLAT)/sksvd0.exe: test/sksvd0x.F90 test/gksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o sksvd0.exe ../../../src/test/sksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG) $(LAPACK)

../bin/$(PLAT)/cksvd0.exe: test/cksvd0x.F90 test/hksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o cksvd0.exe ../../../src/test/cksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG) $(LAPACK)

../bin/$(PLAT)/dksvd0.exe: test/dksvd0x.F90 test/gksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dksvd0.exe ../../../src/test/dksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG) $(LAPACK)

../bin/$(PLAT)/zksvd0.exe: test/zksvd0x.F90 test/hksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o zksvd0.exe ../../../src/test/zksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG) $(LAPACK)

../bin/$(PLAT)/qksvd0.exe: test/qksvd0x.F90 test/gksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qksvd0.exe ../../../src/test/qksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG)

../bin/$(PLAT)/yksvd0.exe: test/yksvd0x.F90 test/hksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yksvd0.exe ../../../src/test/yksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG)

../bin/$(PLAT)/xksvd0.exe: test/xksvd0x.F90 test/gksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xksvd0.exe ../../../src/test/xksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG)

../bin/$(PLAT)/wksvd0.exe: test/wksvd0x.F90 test/hksvd0x.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wksvd0.exe ../../../src/test/wksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../JACSD -ljstrat$(DEBUG)
