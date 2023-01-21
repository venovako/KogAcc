../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS) blksvd/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../obj/$(PLAT)/nb2m.o: blksvd/nb2m.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/nb2m.f90

../obj/$(PLAT)/sbrdg.o: blksvd/sbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/sbrdg.f90

../obj/$(PLAT)/cbrdg.o: blksvd/cbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/cbrdg.f90

../obj/$(PLAT)/dbrdg.o: blksvd/dbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/dbrdg.f90

../obj/$(PLAT)/zbrdg.o: blksvd/zbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/zbrdg.f90

../obj/$(PLAT)/qbrdg.o: blksvd/qbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/qbrdg.f90

../obj/$(PLAT)/ybrdg.o: blksvd/ybrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/ybrdg.f90

../obj/$(PLAT)/xbrdg.o: blksvd/xbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/xbrdg.f90

../obj/$(PLAT)/wbrdg.o: blksvd/wbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/wbrdg.f90

../obj/$(PLAT)/sksvd0.o: blksvd/sksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/sksvd0.F90

../obj/$(PLAT)/cksvd0.o: blksvd/cksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/cksvd0.F90

../obj/$(PLAT)/dksvd0.o: blksvd/dksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/dksvd0.F90

../obj/$(PLAT)/zksvd0.o: blksvd/zksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/zksvd0.F90

../obj/$(PLAT)/qksvd0.o: blksvd/qksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/qksvd0.F90

../obj/$(PLAT)/yksvd0.o: blksvd/yksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/yksvd0.F90

../obj/$(PLAT)/xksvd0.o: blksvd/xksvd0.F90 blksvd/gksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/xksvd0.F90

../obj/$(PLAT)/wksvd0.o: blksvd/wksvd0.F90 blksvd/hksvd0.F90 $(MKFS) blksvd/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/wksvd0.F90
