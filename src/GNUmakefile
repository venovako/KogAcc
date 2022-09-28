include system.mk
include $(COMPILER).mk
include $(ARCH).mk
include $(ABI).mk
include $(OS).mk
include debug.mk
MKFS=GNUmakefile system.mk $(COMPILER).mk $(ARCH).mk $(ABI).mk $(OS).mk debug.mk
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk
ifndef PLAT
PLAT=$(COMPILER)/$(OS)-$(ARCH)-$(ABI)$(DEBUG)
endif # !PLAT
OBJS_ARRBIO=../obj/$(PLAT)/bfopen.o ../obj/$(PLAT)/sbrd1.o ../obj/$(PLAT)/cbrd1.o ../obj/$(PLAT)/dbrd1.o ../obj/$(PLAT)/zbrd1.o ../obj/$(PLAT)/sbwr1.o ../obj/$(PLAT)/cbwr1.o ../obj/$(PLAT)/dbwr1.o ../obj/$(PLAT)/zbwr1.o ../obj/$(PLAT)/sbrd2.o ../obj/$(PLAT)/cbrd2.o ../obj/$(PLAT)/dbrd2.o ../obj/$(PLAT)/zbrd2.o ../obj/$(PLAT)/sbwr2.o ../obj/$(PLAT)/cbwr2.o ../obj/$(PLAT)/dbwr2.o ../obj/$(PLAT)/zbwr2.o
OBJS_BLKSVD=../obj/$(PLAT)/sbordg.o ../obj/$(PLAT)/cbordg.o ../obj/$(PLAT)/dbordg.o ../obj/$(PLAT)/zbordg.o ../obj/$(PLAT)/nb2m.o ../obj/$(PLAT)/dsoffsq.o ../obj/$(PLAT)/dcoffsq.o
OBJS_KSVD2=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/cksvd2.o ../obj/$(PLAT)/dksvd2.o ../obj/$(PLAT)/zksvd2.o ../obj/$(PLAT)/slwsv2.o ../obj/$(PLAT)/dlwsv2.o
EXES_KSVD2=../bin/$(PLAT)/sksvd2.exe ../bin/$(PLAT)/cksvd2.exe ../bin/$(PLAT)/dksvd2.exe ../bin/$(PLAT)/zksvd2.exe ../bin/$(PLAT)/slwsv2.exe ../bin/$(PLAT)/dlwsv2.exe
ifneq ($(COMPILER),nvfortran)
OBJS_ARRBIO += ../obj/$(PLAT)/qbrd1.o ../obj/$(PLAT)/qbwr1.o
OBJS_BLKSVD += ../obj/$(PLAT)/qdoffsq.o ../obj/$(PLAT)/qzoffsq.o
ifneq ($(COMPILER),xlf)
OBJS_KSVD2 += ../obj/$(PLAT)/qksvd2.o ../obj/$(PLAT)/yksvd2.o
EXES_KSVD2 += ../bin/$(PLAT)/qksvd2.exe ../bin/$(PLAT)/yksvd2.exe
endif # !xlf
endif # !nvfortran
ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJS_ARRBIO += ../obj/$(PLAT)/xbrd1.o ../obj/$(PLAT)/xbwr1.o
OBJS_BLKSVD += ../obj/$(PLAT)/xdoffsq.o ../obj/$(PLAT)/xzoffsq.o
OBJS_KSVD2 += ../obj/$(PLAT)/xksvd2.o ../obj/$(PLAT)/wksvd2.o
EXES_KSVD2 += ../bin/$(PLAT)/xksvd2.exe ../bin/$(PLAT)/wksvd2.exe
endif # x86
endif # gfortran
OBJS=$(OBJS_ARRBIO) $(OBJS_BLKSVD) $(OBJS_KSVD2)
LIBS=../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libksvd2.a
EXES=$(EXES_KSVD2) ../bin/$(PLAT)/srnd.exe

.PHONY: all clean dirs doc help purge

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx|nvfortran|xlf] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

doc: ../Doxyfile
	cd .. && doxygen && cd src

../lib/$(PLAT)/libarrbio.a: $(OBJS_ARRBIO) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_ARRBIO)

../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../lib/$(PLAT)/libksvd2.a: $(OBJS_KSVD2) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_KSVD2)

../obj/$(PLAT)/sksvd2.o: sksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/cksvd2.o: cksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dksvd2.o: dksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/zksvd2.o: zksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qksvd2.o: qksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/yksvd2.o: yksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ yksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) yksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/xksvd2.o: xksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xksvd2.f90

../obj/$(PLAT)/wksvd2.o: wksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ wksvd2.f90

../obj/$(PLAT)/slwsv2.o: slwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ slwsv2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) slwsv2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dlwsv2.o: dlwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dlwsv2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dlwsv2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/sbordg.o: sbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbordg.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sbordg.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/cbordg.o: cbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbordg.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cbordg.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dbordg.o: dbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbordg.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dbordg.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/zbordg.o: zbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbordg.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zbordg.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/nb2m.o: nb2m.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ nb2m.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) nb2m.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dsoffsq.o: dsoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dsoffsq.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dsoffsq.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dcoffsq.o: dcoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dcoffsq.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dcoffsq.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qdoffsq.o: qdoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qdoffsq.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qdoffsq.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qzoffsq.o: qzoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qzoffsq.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qzoffsq.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/xdoffsq.o: xdoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xdoffsq.f90

../obj/$(PLAT)/xzoffsq.o: xzoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xzoffsq.f90

../obj/$(PLAT)/bfopen.o: bfopen.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ bfopen.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) bfopen.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/sbrd1.o: sbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbrd1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sbrd1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/cbrd1.o: cbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbrd1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cbrd1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dbrd1.o: dbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbrd1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dbrd1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/zbrd1.o: zbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbrd1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zbrd1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qbrd1.o: qbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qbrd1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qbrd1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/xbrd1.o: xbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xbrd1.f90

../obj/$(PLAT)/sbwr1.o: sbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbwr1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sbwr1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/cbwr1.o: cbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbwr1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cbwr1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dbwr1.o: dbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbwr1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dbwr1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/zbwr1.o: zbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbwr1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zbwr1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qbwr1.o: qbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qbwr1.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qbwr1.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/xbwr1.o: xbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xbwr1.f90

../obj/$(PLAT)/sbrd2.o: sbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbrd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sbrd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/cbrd2.o: cbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbrd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cbrd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dbrd2.o: dbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbrd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dbrd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/zbrd2.o: zbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbrd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zbrd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/sbwr2.o: sbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbwr2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sbwr2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/cbwr2.o: cbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbwr2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cbwr2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dbwr2.o: dbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbwr2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dbwr2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/zbwr2.o: zbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbwr2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zbwr2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/sksvd2.exe: sksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ sksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/cksvd2.exe: cksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ cksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/dksvd2.exe: dksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/zksvd2.exe: zksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ zksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/qksvd2.exe: qksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ qksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/yksvd2.exe: yksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ yksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) yksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/xksvd2.exe: xksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ xksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/wksvd2.exe: wksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ wksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/slwsv2.exe: slwsv2x.f90 tlasv2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ slwsv2x.f90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) slwsv2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/dlwsv2.exe: dlwsv2x.f90 tlasv2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dlwsv2x.f90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dlwsv2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/srnd.exe: srnd.f90 $(MKFS)
	$(FC) $(FCFLAGS) -o $@ srnd.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) srnd.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

clean:
	-$(DEL) $(EXES) $(LIBS) $(OBJS)

purge:
	-$(DEL) ../bin ../lib ../obj ../doc
