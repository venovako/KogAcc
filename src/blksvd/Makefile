..\lib\$(PLAT)\blksvd.lib: $(OBJS_BLKSVD) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_BLKSVD)

..\obj\$(PLAT)\nb2m.obj: blksvd\nb2m.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\nb2m.F90

..\obj\$(PLAT)\sbrdg.obj: blksvd\sbrdg.F90 blksvd\gbrdg.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbrdg.F90

..\obj\$(PLAT)\cbrdg.obj: blksvd\cbrdg.F90 blksvd\gbrdg.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbrdg.F90

..\obj\$(PLAT)\dbrdg.obj: blksvd\dbrdg.F90 blksvd\gbrdg.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbrdg.F90

..\obj\$(PLAT)\zbrdg.obj: blksvd\zbrdg.F90 blksvd\gbrdg.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbrdg.F90

..\obj\$(PLAT)\sbpack.obj: blksvd\sbpack.F90 blksvd\gbpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbpack.F90

..\obj\$(PLAT)\cbpack.obj: blksvd\cbpack.F90 blksvd\gbpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbpack.F90

..\obj\$(PLAT)\dbpack.obj: blksvd\dbpack.F90 blksvd\gbpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbpack.F90

..\obj\$(PLAT)\zbpack.obj: blksvd\zbpack.F90 blksvd\gbpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbpack.F90

..\obj\$(PLAT)\sbunpack.obj: blksvd\sbunpack.F90 blksvd\gbunpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbunpack.F90

..\obj\$(PLAT)\cbunpack.obj: blksvd\cbunpack.F90 blksvd\gbunpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbunpack.F90

..\obj\$(PLAT)\dbunpack.obj: blksvd\dbunpack.F90 blksvd\gbunpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbunpack.F90

..\obj\$(PLAT)\zbunpack.obj: blksvd\zbunpack.F90 blksvd\gbunpack.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbunpack.F90

..\obj\$(PLAT)\sbupdate.obj: blksvd\sbupdate.F90 blksvd\gbupdate.F90 blksvd\gbupGc.F90 blksvd\gbupGr.F90 blksvd\gbupU.F90 blksvd\gbupV.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbupdate.F90

..\obj\$(PLAT)\cbupdate.obj: blksvd\cbupdate.F90 blksvd\gbupdate.F90 blksvd\gbupGc.F90 blksvd\gbupGr.F90 blksvd\gbupU.F90 blksvd\gbupV.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbupdate.F90

..\obj\$(PLAT)\dbupdate.obj: blksvd\dbupdate.F90 blksvd\gbupdate.F90 blksvd\gbupGc.F90 blksvd\gbupGr.F90 blksvd\gbupU.F90 blksvd\gbupV.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbupdate.F90

..\obj\$(PLAT)\zbupdate.obj: blksvd\zbupdate.F90 blksvd\gbupdate.F90 blksvd\gbupGc.F90 blksvd\gbupGr.F90 blksvd\gbupU.F90 blksvd\gbupV.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbupdate.F90

..\obj\$(PLAT)\sksvd0.obj: blksvd\sksvd0.F90 blksvd\gksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sksvd0.F90

..\obj\$(PLAT)\cksvd0.obj: blksvd\cksvd0.F90 blksvd\hksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cksvd0.F90

..\obj\$(PLAT)\dksvd0.obj: blksvd\dksvd0.F90 blksvd\gksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dksvd0.F90

..\obj\$(PLAT)\zksvd0.obj: blksvd\zksvd0.F90 blksvd\hksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zksvd0.F90

..\obj\$(PLAT)\sksvd1.obj: blksvd\sksvd1.F90 blksvd\gksvd1.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sksvd1.F90

..\obj\$(PLAT)\cksvd1.obj: blksvd\cksvd1.F90 blksvd\hksvd1.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cksvd1.F90

..\obj\$(PLAT)\dksvd1.obj: blksvd\dksvd1.F90 blksvd\gksvd1.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dksvd1.F90

..\obj\$(PLAT)\zksvd1.obj: blksvd\zksvd1.F90 blksvd\hksvd1.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zksvd1.F90

..\obj\$(PLAT)\sksvdd.obj: blksvd\sksvdd.F90 blksvd\gksvdd.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sksvdd.F90

..\obj\$(PLAT)\dksvdd.obj: blksvd\dksvdd.F90 blksvd\gksvdd.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dksvdd.F90

..\obj\$(PLAT)\cksvdd.obj: blksvd\cksvdd.F90 blksvd\hksvdd.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cksvdd.F90

..\obj\$(PLAT)\zksvdd.obj: blksvd\zksvdd.F90 blksvd\hksvdd.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zksvdd.F90

..\obj\$(PLAT)\sbksvd0.obj: blksvd\sbksvd0.F90 blksvd\gbksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbksvd0.F90

..\obj\$(PLAT)\dbksvd0.obj: blksvd\dbksvd0.F90 blksvd\gbksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\dbksvd0.F90

..\obj\$(PLAT)\cbksvd0.obj: blksvd\cbksvd0.F90 blksvd\gbksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbksvd0.F90

..\obj\$(PLAT)\zbksvd0.obj: blksvd\zbksvd0.F90 blksvd\gbksvd0.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\zbksvd0.F90

..\obj\$(PLAT)\sbksvdd.obj: blksvd\sbksvdd.F90 blksvd\gbksvdd.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\sbksvdd.F90

..\obj\$(PLAT)\cbksvdd.obj: blksvd\cbksvdd.F90 blksvd\gbksvdd.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\cbksvdd.F90

..\obj\$(PLAT)\ibdims.obj: blksvd\ibdims.F90 $(MKFS)
	cd ..\obj\$(PLAT) && $(FC) $(FCFLAGS) /c ..\..\..\src\blksvd\ibdims.F90
