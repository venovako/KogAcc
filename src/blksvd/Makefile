..\lib\$(PLAT)\blksvd.lib: $(OBJS_BLKSVD) $(MKFS) blksvd\Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_BLKSVD)

..\obj\$(PLAT)\nb2m.obj: blksvd\nb2m.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\nb2m.f90

..\obj\$(PLAT)\sbrdg.obj: blksvd\sbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbrdg.f90

..\obj\$(PLAT)\cbrdg.obj: blksvd\cbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbrdg.f90

..\obj\$(PLAT)\dbrdg.obj: blksvd\dbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbrdg.f90

..\obj\$(PLAT)\zbrdg.obj: blksvd\zbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbrdg.f90

..\obj\$(PLAT)\qbrdg.obj: blksvd\qbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\qbrdg.f90

..\obj\$(PLAT)\ybrdg.obj: blksvd\ybrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\ybrdg.f90

..\obj\$(PLAT)\sksvd0.obj: blksvd\sksvd0.F90 blksvd\gksvd0.F90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /fpp /c ..\..\..\src\blksvd\sksvd0.F90

..\obj\$(PLAT)\cksvd0.obj: blksvd\cksvd0.F90 blksvd\hksvd0.F90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /fpp /c ..\..\..\src\blksvd\cksvd0.F90

..\obj\$(PLAT)\dksvd0.obj: blksvd\dksvd0.F90 blksvd\gksvd0.F90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /fpp /c ..\..\..\src\blksvd\dksvd0.F90

..\obj\$(PLAT)\zksvd0.obj: blksvd\zksvd0.F90 blksvd\hksvd0.F90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /fpp /c ..\..\..\src\blksvd\zksvd0.F90

..\obj\$(PLAT)\qksvd0.obj: blksvd\qksvd0.F90 blksvd\gksvd0.F90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /fpp /c ..\..\..\src\blksvd\qksvd0.F90

..\obj\$(PLAT)\yksvd0.obj: blksvd\yksvd0.F90 blksvd\hksvd0.F90 $(MKFS) blksvd\Makefile
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /fpp /c ..\..\..\src\blksvd\yksvd0.F90
