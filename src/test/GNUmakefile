../bin/$(PLAT)/sksvd2.exe: test/sksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o sksvd2.exe ../../../src/test/sksvd2x.F90 $(OBJS_CRF_MATH) -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/cksvd2.exe: test/cksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o cksvd2.exe ../../../src/test/cksvd2x.F90 $(OBJS_CRF_MATH) -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/dksvd2.exe: test/dksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dksvd2.exe ../../../src/test/dksvd2x.F90 $(OBJS_CRD_MATH) -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/zksvd2.exe: test/zksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o zksvd2.exe ../../../src/test/zksvd2x.F90 $(OBJS_CRD_MATH) -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/qksvd2.exe: test/qksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qksvd2.exe ../../../src/test/qksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/yksvd2.exe: test/yksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yksvd2.exe ../../../src/test/yksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/xksvd2.exe: test/xksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xksvd2.exe ../../../src/test/xksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/wksvd2.exe: test/wksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wksvd2.exe ../../../src/test/wksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 && popd

../bin/$(PLAT)/slwsv2.exe: test/slwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o slwsv2.exe ../../../src/test/slwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LAPACK) && popd

../bin/$(PLAT)/dlwsv2.exe: test/dlwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dlwsv2.exe ../../../src/test/dlwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LAPACK) && popd

../bin/$(PLAT)/spqsort.exe: test/spqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o spqsort.exe ../../../src/test/spqsortx.f90 -L../../../lib/$(PLAT) -ldynord && popd

../bin/$(PLAT)/dpqsort.exe: test/dpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o dpqsort.exe ../../../src/test/dpqsortx.f90 -L../../../lib/$(PLAT) -ldynord && popd

../bin/$(PLAT)/qpqsort.exe: test/qpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qpqsort.exe ../../../src/test/qpqsortx.f90 -L../../../lib/$(PLAT) -ldynord && popd

../bin/$(PLAT)/xpqsort.exe: test/xpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xpqsort.exe ../../../src/test/xpqsortx.f90 -L../../../lib/$(PLAT) -ldynord && popd

../bin/$(PLAT)/srnd.exe: test/srnd.f90 $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o srnd.exe ../../../src/test/srnd.f90 && popd

../bin/$(PLAT)/sthalt.exe: test/sthaltx.f90 ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o sthalt.exe ../../../src/test/sthaltx.f90 -L../../../lib/$(PLAT) -lfaux && popd

../bin/$(PLAT)/jsweep.exe: test/jsweepx.f90 ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	pushd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o jsweep.exe ../../../src/test/jsweepx.f90 -L../../../lib/$(PLAT) -lfaux -L../../../../JACSD -ljstrat$(DEBUG) && popd
