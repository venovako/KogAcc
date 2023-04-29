../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS) blksvd/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../obj/$(PLAT)/nb2m.$(OXT): blksvd/nb2m.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/nb2m.f90

../obj/$(PLAT)/sbrdg.$(OXT): blksvd/sbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sbrdg.f90

../obj/$(PLAT)/cbrdg.$(OXT): blksvd/cbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cbrdg.f90

../obj/$(PLAT)/dbrdg.$(OXT): blksvd/dbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/dbrdg.f90

../obj/$(PLAT)/zbrdg.$(OXT): blksvd/zbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/zbrdg.f90

../obj/$(PLAT)/qbrdg.$(OXT): blksvd/qbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/qbrdg.f90

../obj/$(PLAT)/ybrdg.$(OXT): blksvd/ybrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/ybrdg.f90

../obj/$(PLAT)/xbrdg.$(OXT): blksvd/xbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/xbrdg.f90

../obj/$(PLAT)/wbrdg.$(OXT): blksvd/wbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/wbrdg.f90

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

../obj/$(PLAT)/sksumd.$(OXT): blksvd/sksumd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/sksumd.F90

../obj/$(PLAT)/cksvdd.$(OXT): blksvd/cksvdd.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/blksvd/cksvdd.F90
