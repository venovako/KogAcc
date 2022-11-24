../lib/$(PLAT)/libksvd2.a: $(OBJS_KSVD2) $(MKFS) ksvd2/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_KSVD2)

../obj/$(PLAT)/sksvd2.o: ksvd2/sksvd2.F90 ksvd2/gksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/sksvd2.F90

../obj/$(PLAT)/cksvd2.o: ksvd2/cksvd2.F90 ksvd2/hksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/cksvd2.F90

../obj/$(PLAT)/dksvd2.o: ksvd2/dksvd2.F90 ksvd2/gksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/dksvd2.F90

../obj/$(PLAT)/zksvd2.o: ksvd2/zksvd2.F90 ksvd2/hksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/zksvd2.F90

../obj/$(PLAT)/qksvd2.o: ksvd2/qksvd2.F90 ksvd2/gksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/qksvd2.F90

../obj/$(PLAT)/yksvd2.o: ksvd2/yksvd2.F90 ksvd2/hksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/yksvd2.F90

../obj/$(PLAT)/xksvd2.o: ksvd2/xksvd2.F90 ksvd2/gksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/xksvd2.F90

../obj/$(PLAT)/wksvd2.o: ksvd2/wksvd2.F90 ksvd2/hksvd2$(INTRIN).F90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/wksvd2.F90

../obj/$(PLAT)/slwsv2.o: ksvd2/slwsv2.f90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/slwsv2.f90

../obj/$(PLAT)/dlwsv2.o: ksvd2/dlwsv2.f90 $(MKFS) ksvd2/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ ksvd2/dlwsv2.f90
