../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS) blksvd/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../obj/$(PLAT)/sbrdg.o: blksvd/sbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/sbrdg.f90

../obj/$(PLAT)/cbrdg.o: blksvd/cbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/cbrdg.f90

../obj/$(PLAT)/dbrdg.o: blksvd/dbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/dbrdg.f90

../obj/$(PLAT)/zbrdg.o: blksvd/zbrdg.f90 blksvd/gbrdg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/zbrdg.f90

../obj/$(PLAT)/sbordg.o: blksvd/sbordg.f90 blksvd/gbordg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/sbordg.f90

../obj/$(PLAT)/cbordg.o: blksvd/cbordg.f90 blksvd/gbordg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/cbordg.f90

../obj/$(PLAT)/dbordg.o: blksvd/dbordg.f90 blksvd/gbordg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/dbordg.f90

../obj/$(PLAT)/zbordg.o: blksvd/zbordg.f90 blksvd/gbordg.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/zbordg.f90

../obj/$(PLAT)/nb2m.o: blksvd/nb2m.f90 $(MKFS) blksvd/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ blksvd/nb2m.f90
