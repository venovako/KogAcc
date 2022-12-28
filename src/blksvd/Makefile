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

..\obj\$(PLAT)\sbordg.obj: blksvd\sbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbordg.f90 && popd

..\obj\$(PLAT)\cbordg.obj: blksvd\cbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbordg.f90 && popd

..\obj\$(PLAT)\dbordg.obj: blksvd\dbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbordg.f90 && popd

..\obj\$(PLAT)\zbordg.obj: blksvd\zbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbordg.f90 && popd

..\obj\$(PLAT)\qbordg.obj: blksvd\qbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\qbordg.f90 && popd

..\obj\$(PLAT)\ybordg.obj: blksvd\ybordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\ybordg.f90 && popd

..\obj\$(PLAT)\nb2m.obj: blksvd\nb2m.f90 $(MKFS) blksvd\Makefile
	pushd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\nb2m.f90 && popd
