../bin/$(PLAT)/xksvd2.exe: test/xksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xksvd2.exe ../../../src/test/xksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/wksvd2.exe: test/wksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wksvd2.exe ../../../src/test/wksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xlwsv2.exe: test/xlwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xlwsv2.exe ../../../src/test/xlwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qpqsort.exe: test/qpqsortx.F90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qpqsort.exe ../../../src/test/qpqsortx.F90 -L../../../lib/$(PLAT) -ldynord -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xpqsort.exe: test/xpqsortx.F90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xpqsort.exe ../../../src/test/xpqsortx.F90 -L../../../lib/$(PLAT) -ldynord -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qran.exe: test/qran.F90 test/gran.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qran.exe ../../../src/test/qran.F90 -L../../../lib/$(PLAT) -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/yran.exe: test/yran.F90 test/hran.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yran.exe ../../../src/test/yran.F90 -L../../../lib/$(PLAT) -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xran.exe: test/xran.F90 test/gran.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xran.exe ../../../src/test/xran.F90 -L../../../lib/$(PLAT) -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/wran.exe: test/wran.F90 test/hran.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wran.exe ../../../src/test/wran.F90 -L../../../lib/$(PLAT) -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qrnd.exe: test/qrnd.F90 test/grnd.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qrnd.exe ../../../src/test/qrnd.F90 -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/yrnd.exe: test/yrnd.F90 test/hrnd.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yrnd.exe ../../../src/test/yrnd.F90 -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xrnd.exe: test/xrnd.F90 test/grnd.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xrnd.exe ../../../src/test/xrnd.F90 -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/wrnd.exe: test/wrnd.F90 test/hrnd.F90 $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wrnd.exe ../../../src/test/wrnd.F90 -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qran2u.exe: test/qran2t.F90 test/gran2t.F90 test/gran2u.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -DUPPER -o qran2u.exe ../../../src/test/qran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qran2g.exe: test/qran2t.F90 test/gran2t.F90 test/gran2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qran2g.exe ../../../src/test/qran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/yran2g.exe: test/yran2t.F90 test/hran2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yran2g.exe ../../../src/test/yran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xran2u.exe: test/xran2t.F90 test/gran2t.F90 test/gran2u.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -DUPPER -o xran2u.exe ../../../src/test/xran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xran2g.exe: test/xran2t.F90 test/gran2t.F90 test/gran2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xran2g.exe ../../../src/test/xran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/wran2g.exe: test/wran2t.F90 test/hran2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wran2g.exe ../../../src/test/wran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qrnd2u.exe: test/qrnd2t.F90 test/grnd2t.F90 test/grnd2u.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -DUPPER -o qrnd2u.exe ../../../src/test/qrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qrnd2g.exe: test/qrnd2t.F90 test/grnd2t.F90 test/grnd2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qrnd2g.exe ../../../src/test/qrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/yrnd2g.exe: test/yrnd2t.F90 test/hrnd2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yrnd2g.exe ../../../src/test/yrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xrnd2u.exe: test/xrnd2t.F90 test/grnd2t.F90 test/grnd2u.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -DUPPER -o xrnd2u.exe ../../../src/test/xrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xrnd2g.exe: test/xrnd2t.F90 test/grnd2t.F90 test/grnd2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xrnd2g.exe ../../../src/test/xrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/wrnd2g.exe: test/wrnd2t.F90 test/hrnd2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wrnd2g.exe ../../../src/test/wrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/qksvd0.exe: test/qksvd0x.F90 test/gksvd0x.F90 test/qrdinp.F90 test/qwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o qksvd0.exe ../../../src/test/qksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/yksvd0.exe: test/yksvd0x.F90 test/hksvd0x.F90 test/yrdinp.F90 test/ywrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o yksvd0.exe ../../../src/test/yksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../libpvn/src -lpvn -ldl

../bin/$(PLAT)/xksvd0.exe: test/xksvd0x.F90 test/gksvd0x.F90 test/xrdinp.F90 test/xwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o xksvd0.exe ../../../src/test/xksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../libpvn/src -lpvn $(GNU) -ldl

../bin/$(PLAT)/wksvd0.exe: test/wksvd0x.F90 test/hksvd0x.F90 test/wrdinp.F90 test/wwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FCFLAGS) -o wksvd0.exe ../../../src/test/wksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux -L../../../../libpvn/src -lpvn $(GNU) -ldl
