../bin/$(PLAT)/sksvd2.exe: test/sksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sksvd2.exe ../../../src/test/sksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/cksvd2.exe: test/cksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cksvd2.exe ../../../src/test/cksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/dksvd2.exe: test/dksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dksvd2.exe ../../../src/test/dksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/zksvd2.exe: test/zksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zksvd2.exe ../../../src/test/zksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/qksvd2.exe: test/qksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o qksvd2.exe ../../../src/test/qksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/yksvd2.exe: test/yksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o yksvd2.exe ../../../src/test/yksvd2x.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/slwsv2.exe: test/slwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o slwsv2.exe ../../../src/test/slwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LDFLAGS)

../bin/$(PLAT)/dlwsv2.exe: test/dlwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dlwsv2.exe ../../../src/test/dlwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LDFLAGS)

../bin/$(PLAT)/qlwsv2.exe: test/qlwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o qlwsv2.exe ../../../src/test/qlwsv2x.F90 -L../../../lib/$(PLAT) -lksvd2 $(LDFLAGS)

../bin/$(PLAT)/sran.exe: test/sran.F90 test/gran.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sran.exe ../../../src/test/sran.F90 -L../../../lib/$(PLAT) -lfaux $(LDFLAGS)

../bin/$(PLAT)/cran.exe: test/cran.F90 test/hran.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cran.exe ../../../src/test/cran.F90 -L../../../lib/$(PLAT) -lfaux $(LDFLAGS)

../bin/$(PLAT)/dran.exe: test/dran.F90 test/gran.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dran.exe ../../../src/test/dran.F90 -L../../../lib/$(PLAT) -lfaux $(LDFLAGS)

../bin/$(PLAT)/zran.exe: test/zran.F90 test/hran.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zran.exe ../../../src/test/zran.F90 -L../../../lib/$(PLAT) -lfaux $(LDFLAGS)

../bin/$(PLAT)/srnd.exe: test/srnd.F90 test/grnd.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o srnd.exe ../../../src/test/srnd.F90 $(LDFLAGS)

../bin/$(PLAT)/crnd.exe: test/crnd.F90 test/hrnd.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o crnd.exe ../../../src/test/crnd.F90 $(LDFLAGS)

../bin/$(PLAT)/drnd.exe: test/drnd.F90 test/grnd.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o drnd.exe ../../../src/test/drnd.F90 $(LDFLAGS)

../bin/$(PLAT)/zrnd.exe: test/zrnd.F90 test/hrnd.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zrnd.exe ../../../src/test/zrnd.F90 $(LDFLAGS)

../bin/$(PLAT)/sran2u.exe: test/sran2t.F90 test/gran2t.F90 test/gran2u.F90 test/gr2ud.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -DUPPER -o sran2u.exe ../../../src/test/sran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/sran2g.exe: test/sran2t.F90 test/gran2t.F90 test/gran2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sran2g.exe ../../../src/test/sran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/cran2g.exe: test/cran2t.F90 test/hran2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cran2g.exe ../../../src/test/cran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/dran2u.exe: test/dran2t.F90 test/gran2t.F90 test/gran2u.F90 test/gr2ud.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -DUPPER -o dran2u.exe ../../../src/test/dran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/dran2g.exe: test/dran2t.F90 test/gran2t.F90 test/gran2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dran2g.exe ../../../src/test/dran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/zran2g.exe: test/zran2t.F90 test/hran2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zran2g.exe ../../../src/test/zran2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/srnd2u.exe: test/srnd2t.F90 test/grnd2t.F90 test/grnd2u.F90 test/gr2ud.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -DUPPER -o srnd2u.exe ../../../src/test/srnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/srnd2g.exe: test/srnd2t.F90 test/grnd2t.F90 test/grnd2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o srnd2g.exe ../../../src/test/srnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/crnd2g.exe: test/crnd2t.F90 test/hrnd2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o crnd2g.exe ../../../src/test/crnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/drnd2u.exe: test/drnd2t.F90 test/grnd2t.F90 test/grnd2u.F90 test/gr2ud.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -DUPPER -o drnd2u.exe ../../../src/test/drnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/drnd2g.exe: test/drnd2t.F90 test/grnd2t.F90 test/grnd2g.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o drnd2g.exe ../../../src/test/drnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/zrnd2g.exe: test/zrnd2t.F90 test/hrnd2t.F90 ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zrnd2g.exe ../../../src/test/zrnd2t.F90 -L../../../lib/$(PLAT) -lksvd2 -lfaux $(LDFLAGS)

../bin/$(PLAT)/seedsz.exe: test/seedsz.F90 $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o seedsz.exe ../../../src/test/seedsz.F90 $(LDFLAGS)

../bin/$(PLAT)/sthalt.exe: test/sthaltx.F90 ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sthalt.exe ../../../src/test/sthaltx.F90 -L../../../lib/$(PLAT) -lfaux $(LDFLAGS)

../bin/$(PLAT)/jsweep.exe: test/jsweepx.F90 ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o jsweep.exe ../../../src/test/jsweepx.F90 -L../../../lib/$(PLAT) -lfaux $(LDFLAGS)

../bin/$(PLAT)/sksvd0.exe: test/sksvd0x.F90 test/gksvd0x.F90 test/srdinp.F90 test/swrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sksvd0.exe ../../../src/test/sksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/cksvd0.exe: test/cksvd0x.F90 test/hksvd0x.F90 test/crdinp.F90 test/cwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cksvd0.exe ../../../src/test/cksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/dksvd0.exe: test/dksvd0x.F90 test/gksvd0x.F90 test/drdinp.F90 test/dwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dksvd0.exe ../../../src/test/dksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/zksvd0.exe: test/zksvd0x.F90 test/hksvd0x.F90 test/zrdinp.F90 test/zwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zksvd0.exe ../../../src/test/zksvd0x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/sksvd1.exe: test/sksvd1x.F90 test/gksvd1x.F90 test/srdinp.F90 test/swrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sksvd1.exe ../../../src/test/sksvd1x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/cksvd1.exe: test/cksvd1x.F90 test/hksvd1x.F90 test/crdinp.F90 test/cwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cksvd1.exe ../../../src/test/cksvd1x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/dksvd1.exe: test/dksvd1x.F90 test/gksvd1x.F90 test/drdinp.F90 test/dwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dksvd1.exe ../../../src/test/dksvd1x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/zksvd1.exe: test/zksvd1x.F90 test/hksvd1x.F90 test/zrdinp.F90 test/zwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zksvd1.exe ../../../src/test/zksvd1x.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/sksvdd.exe: test/sksvddx.F90 test/gksvddx.F90 test/srdinp.F90 test/swrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o sksvdd.exe ../../../src/test/sksvddx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/dksvdd.exe: test/dksvddx.F90 test/gksvddx.F90 test/drdinp.F90 test/dwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dksvdd.exe ../../../src/test/dksvddx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/xksvdd.exe: test/xksvddx.F90 test/gksvddx.F90 test/drdinp.F90 test/dwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o xksvdd.exe ../../../src/test/xksvddx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/cksvdd.exe: test/cksvddx.F90 test/hksvddx.F90 test/crdinp.F90 test/cwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cksvdd.exe ../../../src/test/cksvddx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/zksvdd.exe: test/zksvddx.F90 test/hksvddx.F90 test/zrdinp.F90 test/zwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zksvdd.exe ../../../src/test/zksvddx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/wksvdd.exe: test/wksvddx.F90 test/hksvddx.F90 test/zrdinp.F90 test/zwrout.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libfaux.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o wksvdd.exe ../../../src/test/wksvddx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -lksvd2 -larrbio -lfaux $(LDFLAGS)

../bin/$(PLAT)/smkbpq.exe: test/smkbpqx.F90 test/gmkbpqx.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libarrbio.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o smkbpq.exe ../../../src/test/smkbpqx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -larrbio $(LDFLAGS)

../bin/$(PLAT)/dmkbpq.exe: test/dmkbpqx.F90 test/gmkbpqx.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libarrbio.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dmkbpq.exe ../../../src/test/dmkbpqx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -larrbio $(LDFLAGS)

../bin/$(PLAT)/cmkbpq.exe: test/cmkbpqx.F90 test/gmkbpqx.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libarrbio.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o cmkbpq.exe ../../../src/test/cmkbpqx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -larrbio $(LDFLAGS)

../bin/$(PLAT)/zmkbpq.exe: test/zmkbpqx.F90 test/gmkbpqx.F90 ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libarrbio.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o zmkbpq.exe ../../../src/test/zmkbpqx.F90 -L../../../lib/$(PLAT) -lblksvd -ldynord -larrbio $(LDFLAGS)

../bin/$(PLAT)/ibdims.exe: test/ibdimsx.F90 ../lib/$(PLAT)/libblksvd.a $(MKFS)
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o ibdims.exe ../../../src/test/ibdimsx.F90 -L../../../lib/$(PLAT) -lblksvd $(LDFLAGS)
