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

../obj/$(PLAT)/sosum4.o: faux/sosum4.F90 faux/gosum4.F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/sosum4.F90

../obj/$(PLAT)/dosum4.o: faux/dosum4.F90 faux/gosum4.F90 $(MKFS) faux/GNUmakefile
	$(FC) $(FCFLAGS) -c -o $@ faux/dosum4.F90
