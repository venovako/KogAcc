../lib/$(PLAT)/libfaux.a: $(OBJS_FAUX) $(MKFS) faux/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_FAUX)

../obj/$(PLAT)/sthalt.o: faux/sthalt.F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/sthalt.F90

../obj/$(PLAT)/slango.o: faux/slango.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/slango.f90

../obj/$(PLAT)/clango.o: faux/clango.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/clango.f90

../obj/$(PLAT)/dlango.o: faux/dlango.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/dlango.f90

../obj/$(PLAT)/zlango.o: faux/zlango.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/zlango.f90

../obj/$(PLAT)/dsoffsq.o: faux/dsoffsq.F90 faux/goffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/dsoffsq.F90

../obj/$(PLAT)/dcoffsq.o: faux/dcoffsq.F90 faux/hoffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/dcoffsq.F90

../obj/$(PLAT)/qdoffsq.o: faux/qdoffsq.F90 faux/goffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/qdoffsq.F90

../obj/$(PLAT)/qzoffsq.o: faux/qzoffsq.F90 faux/hoffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/qzoffsq.F90

../obj/$(PLAT)/xdoffsq.o: faux/xdoffsq.F90 faux/goffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/xdoffsq.F90

../obj/$(PLAT)/xzoffsq.o: faux/xzoffsq.F90 faux/hoffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/xzoffsq.F90

../obj/$(PLAT)/qxoffsq.o: faux/qxoffsq.F90 faux/goffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/qxoffsq.F90

../obj/$(PLAT)/qwoffsq.o: faux/qwoffsq.F90 faux/hoffsq$(INTRIN).F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/qwoffsq.F90

../obj/$(PLAT)/sosum4.o: faux/sosum4.F90 faux/gosum4.F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/sosum4.F90

../obj/$(PLAT)/dosum4.o: faux/dosum4.F90 faux/gosum4.F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/dosum4.F90

../obj/$(PLAT)/spsum4.o: faux/spsum4.f90 faux/gpsum4.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/spsum4.f90

../obj/$(PLAT)/dpsum4.o: faux/dpsum4.f90 faux/gpsum4.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/dpsum4.f90

../obj/$(PLAT)/qpsum4.o: faux/qpsum4.f90 faux/gpsum4.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/qpsum4.f90

../obj/$(PLAT)/xpsum4.o: faux/xpsum4.f90 faux/gpsum4.f90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/xpsum4.f90
