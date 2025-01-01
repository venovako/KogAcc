../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS) blksvd/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../obj/$(PLAT)/nb2m.$(OXT): blksvd/nb2m.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/nb2m.F90

../obj/$(PLAT)/sbrdg.$(OXT): blksvd/sbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbrdg.F90

../obj/$(PLAT)/cbrdg.$(OXT): blksvd/cbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbrdg.F90

../obj/$(PLAT)/dbrdg.$(OXT): blksvd/dbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbrdg.F90

../obj/$(PLAT)/zbrdg.$(OXT): blksvd/zbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbrdg.F90

../obj/$(PLAT)/sbpack.$(OXT): blksvd/sbpack.F90 blksvd/gbpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbpack.F90

../obj/$(PLAT)/cbpack.$(OXT): blksvd/cbpack.F90 blksvd/gbpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbpack.F90

../obj/$(PLAT)/dbpack.$(OXT): blksvd/dbpack.F90 blksvd/gbpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbpack.F90

../obj/$(PLAT)/zbpack.$(OXT): blksvd/zbpack.F90 blksvd/gbpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbpack.F90

../obj/$(PLAT)/sbunpack.$(OXT): blksvd/sbunpack.F90 blksvd/gbunpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbunpack.F90

../obj/$(PLAT)/cbunpack.$(OXT): blksvd/cbunpack.F90 blksvd/gbunpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbunpack.F90

../obj/$(PLAT)/dbunpack.$(OXT): blksvd/dbunpack.F90 blksvd/gbunpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbunpack.F90

../obj/$(PLAT)/zbunpack.$(OXT): blksvd/zbunpack.F90 blksvd/gbunpack.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbunpack.F90

../obj/$(PLAT)/sbupdate.$(OXT): blksvd/sbupdate.F90 blksvd/gbupdate.F90 blksvd/gbupGc.F90 blksvd/gbupGr.F90 blksvd/gbupU.F90 blksvd/gbupV.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbupdate.F90

../obj/$(PLAT)/cbupdate.$(OXT): blksvd/cbupdate.F90 blksvd/gbupdate.F90 blksvd/gbupGc.F90 blksvd/gbupGr.F90 blksvd/gbupU.F90 blksvd/gbupV.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbupdate.F90

../obj/$(PLAT)/dbupdate.$(OXT): blksvd/dbupdate.F90 blksvd/gbupdate.F90 blksvd/gbupGc.F90 blksvd/gbupGr.F90 blksvd/gbupU.F90 blksvd/gbupV.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbupdate.F90

../obj/$(PLAT)/zbupdate.$(OXT): blksvd/zbupdate.F90 blksvd/gbupdate.F90 blksvd/gbupGc.F90 blksvd/gbupGr.F90 blksvd/gbupU.F90 blksvd/gbupV.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbupdate.F90

../obj/$(PLAT)/sksvd0.$(OXT): blksvd/sksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sksvd0.F90

../obj/$(PLAT)/cksvd0.$(OXT): blksvd/cksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cksvd0.F90

../obj/$(PLAT)/dksvd0.$(OXT): blksvd/dksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dksvd0.F90

../obj/$(PLAT)/zksvd0.$(OXT): blksvd/zksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zksvd0.F90

../obj/$(PLAT)/sksvd1.$(OXT): blksvd/sksvd1.F90 blksvd/gksvd1.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sksvd1.F90

../obj/$(PLAT)/cksvd1.$(OXT): blksvd/cksvd1.F90 blksvd/hksvd1.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cksvd1.F90

../obj/$(PLAT)/dksvd1.$(OXT): blksvd/dksvd1.F90 blksvd/gksvd1.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dksvd1.F90

../obj/$(PLAT)/zksvd1.$(OXT): blksvd/zksvd1.F90 blksvd/hksvd1.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zksvd1.F90

../obj/$(PLAT)/sksvdd.$(OXT): blksvd/sksvdd.F90 blksvd/gksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sksvdd.F90

../obj/$(PLAT)/dksvdd.$(OXT): blksvd/dksvdd.F90 blksvd/gksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dksvdd.F90

../obj/$(PLAT)/cksvdd.$(OXT): blksvd/cksvdd.F90 blksvd/hksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cksvdd.F90

../obj/$(PLAT)/zksvdd.$(OXT): blksvd/zksvdd.F90 blksvd/hksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zksvdd.F90

../obj/$(PLAT)/xksvdd.$(OXT): blksvd/xksvdd.F90 blksvd/gksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/xksvdd.F90

../obj/$(PLAT)/wksvdd.$(OXT): blksvd/wksvdd.F90 blksvd/hksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/wksvdd.F90

../obj/$(PLAT)/sbksvd0.$(OXT): blksvd/sbksvd0.F90 blksvd/gbksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbksvd0.F90

../obj/$(PLAT)/dbksvd0.$(OXT): blksvd/dbksvd0.F90 blksvd/gbksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbksvd0.F90

../obj/$(PLAT)/cbksvd0.$(OXT): blksvd/cbksvd0.F90 blksvd/gbksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbksvd0.F90

../obj/$(PLAT)/zbksvd0.$(OXT): blksvd/zbksvd0.F90 blksvd/gbksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbksvd0.F90

../obj/$(PLAT)/sbksvdd.$(OXT): blksvd/sbksvdd.F90 blksvd/gbksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbksvdd.F90

../obj/$(PLAT)/dbksvdd.$(OXT): blksvd/dbksvdd.F90 blksvd/gbksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbksvdd.F90

../obj/$(PLAT)/cbksvdd.$(OXT): blksvd/cbksvdd.F90 blksvd/gbksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbksvdd.F90

../obj/$(PLAT)/zbksvdd.$(OXT): blksvd/zbksvdd.F90 blksvd/gbksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbksvdd.F90

../obj/$(PLAT)/ibdims.$(OXT): blksvd/ibdims.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/ibdims.F90
