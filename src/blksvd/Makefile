..\lib\$(PLAT)\blksvd.lib: $(OBJS_BLKSVD) $(MKFS) blksvd\Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_BLKSVD)

..\obj\$(PLAT)\sbrdg.obj: blksvd\sbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbrdg.f90 && popd

..\obj\$(PLAT)\cbrdg.obj: blksvd\cbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbrdg.f90 && popd

..\obj\$(PLAT)\dbrdg.obj: blksvd\dbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbrdg.f90 && popd

..\obj\$(PLAT)\zbrdg.obj: blksvd\zbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbrdg.f90 && popd

..\obj\$(PLAT)\qbrdg.obj: blksvd\qbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\qbrdg.f90 && popd

..\obj\$(PLAT)\ybrdg.obj: blksvd\ybrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\ybrdg.f90 && popd

..\obj\$(PLAT)\nb2m.obj: blksvd\nb2m.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\nb2m.f90 && popd
