../lib/$(PLAT)/libdynord.a: $(OBJS_DYNORD) $(MKFS) dynord/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_DYNORD)

../obj/$(PLAT)/spqcmp.o: dynord/spqcmp.f90 dynord/gpqcmp.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/spqcmp.f90

../obj/$(PLAT)/dpqcmp.o: dynord/dpqcmp.f90 dynord/gpqcmp.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/dpqcmp.f90

../obj/$(PLAT)/qpqcmp.o: dynord/qpqcmp.f90 dynord/gpqcmp.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/qpqcmp.f90

../obj/$(PLAT)/xpqcmp.o: dynord/xpqcmp.f90 dynord/gpqcmp.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/xpqcmp.f90

../obj/$(PLAT)/spqmrg.o: dynord/spqmrg.f90 dynord/gpqmrg.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/spqmrg.f90

../obj/$(PLAT)/dpqmrg.o: dynord/dpqmrg.f90 dynord/gpqmrg.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/dpqmrg.f90

../obj/$(PLAT)/qpqmrg.o: dynord/qpqmrg.f90 dynord/gpqmrg.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/qpqmrg.f90

../obj/$(PLAT)/xpqmrg.o: dynord/xpqmrg.f90 dynord/gpqmrg.f90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/xpqmrg.f90

../obj/$(PLAT)/spqsrt.o: dynord/spqsrt.F90 dynord/gpqsrt.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/spqsrt.F90

../obj/$(PLAT)/dpqsrt.o: dynord/dpqsrt.F90 dynord/gpqsrt.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/dpqsrt.F90

../obj/$(PLAT)/spqsort.o: dynord/spqsort.F90 dynord/gpqsort.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/spqsort.F90

../obj/$(PLAT)/dpqsort.o: dynord/dpqsort.F90 dynord/gpqsort.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/dpqsort.F90

../obj/$(PLAT)/qpqsrt.o: dynord/qpqsrt.F90 dynord/gpqsrt.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/qpqsrt.F90

../obj/$(PLAT)/qpqsort.o: dynord/qpqsort.F90 dynord/gpqsort.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/qpqsort.F90

../obj/$(PLAT)/xpqsrt.o: dynord/xpqsrt.F90 dynord/gpqsrt.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/xpqsrt.F90

../obj/$(PLAT)/xpqsort.o: dynord/xpqsort.F90 dynord/gpqsort.F90 $(MKFS) dynord/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ dynord/xpqsort.F90
