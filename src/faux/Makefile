..\lib\$(PLAT)\faux.lib: $(OBJS_FAUX) $(MKFS) faux\Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_FAUX)

..\obj\$(PLAT)\sthalt.obj: faux\sthalt.F90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /fpp /c faux\sthalt.F90 /Fo$@

..\obj\$(PLAT)\slango.obj: faux\slango.f90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /c faux\slango.f90 /Fo$@

..\obj\$(PLAT)\clango.obj: faux\clango.f90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /c faux\clango.f90 /Fo$@

..\obj\$(PLAT)\dlango.obj: faux\dlango.f90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /c faux\dlango.f90 /Fo$@

..\obj\$(PLAT)\zlango.obj: faux\zlango.f90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /c faux\zlango.f90 /Fo$@

..\obj\$(PLAT)\sosum4.obj: faux\sosum4.F90 faux\gosum4.F90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /fpp /c faux\sosum4.F90 /Fo$@

..\obj\$(PLAT)\dosum4.obj: faux\dosum4.F90 faux\gosum4.F90 $(MKFS) faux\Makefile
	$(FC) $(FCFLAGS) /fpp /c faux\dosum4.F90 /Fo$@
