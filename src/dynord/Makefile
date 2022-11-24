..\lib\$(PLAT)\dynord.lib: $(OBJS_DYNORD) $(MKFS) dynord\Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_DYNORD)

..\obj\$(PLAT)\spqcmp.obj: dynord\spqcmp.f90 dynord\gpqcmp.f90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /c dynord\spqcmp.f90 /Fo$@

..\obj\$(PLAT)\dpqcmp.obj: dynord\dpqcmp.f90 dynord\gpqcmp.f90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /c dynord\dpqcmp.f90 /Fo$@

..\obj\$(PLAT)\qpqcmp.obj: dynord\qpqcmp.f90 dynord\gpqcmp.f90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /c dynord\qpqcmp.f90 /Fo$@

..\obj\$(PLAT)\spqmrg.obj: dynord\spqmrg.f90 dynord\gpqmrg.f90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /c dynord\spqmrg.f90 /Fo$@

..\obj\$(PLAT)\dpqmrg.obj: dynord\dpqmrg.f90 dynord\gpqmrg.f90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /c dynord\dpqmrg.f90 /Fo$@

..\obj\$(PLAT)\qpqmrg.obj: dynord\qpqmrg.f90 dynord\gpqmrg.f90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /c dynord\qpqmrg.f90 /Fo$@

..\obj\$(PLAT)\spqsrt.obj: dynord\spqsrt.F90 dynord\gpqsrt.F90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /fpp /c dynord\spqsrt.F90 /Fo$@

..\obj\$(PLAT)\dpqsrt.obj: dynord\dpqsrt.F90 dynord\gpqsrt.F90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /fpp /c dynord\dpqsrt.F90 /Fo$@

..\obj\$(PLAT)\qpqsrt.obj: dynord\qpqsrt.F90 dynord\gpqsrt.F90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /fpp /c dynord\qpqsrt.F90 /Fo$@

..\obj\$(PLAT)\spqsort.obj: dynord\spqsort.F90 dynord\gpqsort.F90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /fpp /c dynord\spqsort.F90 /Fo$@

..\obj\$(PLAT)\dpqsort.obj: dynord\dpqsort.F90 dynord\gpqsort.F90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /fpp /c dynord\dpqsort.F90 /Fo$@

..\obj\$(PLAT)\qpqsort.obj: dynord\qpqsort.F90 dynord\gpqsort.F90 $(MKFS) dynord\Makefile
	$(FC) $(FCFLAGS) /fpp /c dynord\qpqsort.F90 /Fo$@
