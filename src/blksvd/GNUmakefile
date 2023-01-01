../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS) blksvd/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../obj/$(PLAT)/sbrdg.o: blksvd/sbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/sbrdg.f90 && popd

../obj/$(PLAT)/cbrdg.o: blksvd/cbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/cbrdg.f90 && popd

../obj/$(PLAT)/dbrdg.o: blksvd/dbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/dbrdg.f90 && popd

../obj/$(PLAT)/zbrdg.o: blksvd/zbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/zbrdg.f90 && popd

../obj/$(PLAT)/qbrdg.o: blksvd/qbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/qbrdg.f90 && popd

../obj/$(PLAT)/ybrdg.o: blksvd/ybrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/ybrdg.f90 && popd

../obj/$(PLAT)/xbrdg.o: blksvd/xbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/xbrdg.f90 && popd

../obj/$(PLAT)/wbrdg.o: blksvd/wbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/wbrdg.f90 && popd

../obj/$(PLAT)/nb2m.o: blksvd/nb2m.f90 $(MKFS) blksvd/GNUmakefile
	pushd ../obj/$(PLAT) && $(FC) $(FCFLAGS) -c ../../../src/blksvd/nb2m.f90 && popd
