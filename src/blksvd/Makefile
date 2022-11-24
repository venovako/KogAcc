..\lib\$(PLAT)\blksvd.lib: $(OBJS_BLKSVD) $(MKFS) blksvd\Makefile
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_BLKSVD)

..\obj\$(PLAT)\sbrdg.obj: blksvd\sbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\sbrdg.f90 /Fo$@

..\obj\$(PLAT)\cbrdg.obj: blksvd\cbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\cbrdg.f90 /Fo$@

..\obj\$(PLAT)\dbrdg.obj: blksvd\dbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\dbrdg.f90 /Fo$@

..\obj\$(PLAT)\zbrdg.obj: blksvd\zbrdg.f90 blksvd\gbrdg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\zbrdg.f90 /Fo$@

..\obj\$(PLAT)\sbordg.obj: blksvd\sbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\sbordg.f90 /Fo$@

..\obj\$(PLAT)\cbordg.obj: blksvd\cbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\cbordg.f90 /Fo$@

..\obj\$(PLAT)\dbordg.obj: blksvd\dbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\dbordg.f90 /Fo$@

..\obj\$(PLAT)\zbordg.obj: blksvd\zbordg.f90 blksvd\gbordg.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\zbordg.f90 /Fo$@

..\obj\$(PLAT)\nb2m.obj: blksvd\nb2m.f90 $(MKFS) blksvd\Makefile
	$(FC) $(FCFLAGS) /c blksvd\nb2m.f90 /Fo$@
