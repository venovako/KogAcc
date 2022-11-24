..\lib\$(PLAT)\ksvd2.lib: $(OBJS_KSVD2) $(MKFS) ksvd2\Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_KSVD2)

..\obj\$(PLAT)\sksvd2.obj: ksvd2\sksvd2.F90 ksvd2\gksvd2$(INTRIN).F90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /fpp /c ksvd2\sksvd2.F90 /Fo$@

..\obj\$(PLAT)\cksvd2.obj: ksvd2\cksvd2.F90 ksvd2\hksvd2$(INTRIN).F90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /fpp /c ksvd2\cksvd2.F90 /Fo$@

..\obj\$(PLAT)\dksvd2.obj: ksvd2\dksvd2.F90 ksvd2\gksvd2$(INTRIN).F90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /fpp /c ksvd2\dksvd2.F90 /Fo$@

..\obj\$(PLAT)\zksvd2.obj: ksvd2\zksvd2.F90 ksvd2\hksvd2$(INTRIN).F90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /fpp /c ksvd2\zksvd2.F90 /Fo$@

..\obj\$(PLAT)\qksvd2.obj: ksvd2\qksvd2.F90 ksvd2\gksvd2$(INTRIN).F90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /fpp /c ksvd2\qksvd2.F90 /Fo$@

..\obj\$(PLAT)\yksvd2.obj: ksvd2\yksvd2.F90 ksvd2\hksvd2$(INTRIN).F90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /fpp /c ksvd2\yksvd2.F90 /Fo$@

..\obj\$(PLAT)\slwsv2.obj: ksvd2\slwsv2.f90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /c ksvd2\slwsv2.f90 /Fo$@

..\obj\$(PLAT)\dlwsv2.obj: ksvd2\dlwsv2.f90 $(MKFS) ksvd2\Makefile
	$(FC) $(FCFLAGS) /c ksvd2\dlwsv2.f90 /Fo$@
