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

../obj/$(PLAT)/qbrdg.$(OXT): blksvd/qbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/qbrdg.F90

../obj/$(PLAT)/ybrdg.$(OXT): blksvd/ybrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/ybrdg.F90

../obj/$(PLAT)/xbrdg.$(OXT): blksvd/xbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/xbrdg.F90

../obj/$(PLAT)/wbrdg.$(OXT): blksvd/wbrdg.F90 blksvd/gbrdg.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/wbrdg.F90

../obj/$(PLAT)/sksvd0.$(OXT): blksvd/sksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sksvd0.F90

../obj/$(PLAT)/cksvd0.$(OXT): blksvd/cksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cksvd0.F90

../obj/$(PLAT)/dksvd0.$(OXT): blksvd/dksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dksvd0.F90

../obj/$(PLAT)/zksvd0.$(OXT): blksvd/zksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zksvd0.F90

../obj/$(PLAT)/qksvd0.$(OXT): blksvd/qksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/qksvd0.F90

../obj/$(PLAT)/yksvd0.$(OXT): blksvd/yksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/yksvd0.F90

../obj/$(PLAT)/xksvd0.$(OXT): blksvd/xksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/xksvd0.F90

../obj/$(PLAT)/wksvd0.$(OXT): blksvd/wksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/wksvd0.F90

../obj/$(PLAT)/sksvdd.$(OXT): blksvd/sksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sksvdd.F90

../obj/$(PLAT)/dksvdd.$(OXT): blksvd/dksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dksvdd.F90

../obj/$(PLAT)/cksvdd.$(OXT): blksvd/cksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cksvdd.F90

../obj/$(PLAT)/zksvdd.$(OXT): blksvd/zksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zksvdd.F90

../obj/$(PLAT)/xksvdd.$(OXT): blksvd/xksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/xksvdd.F90

../obj/$(PLAT)/wksvdd.$(OXT): blksvd/wksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/wksvdd.F90
