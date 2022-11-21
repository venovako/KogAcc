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
OBJS_ARRBIO=../obj/$(PLAT)/bfopen.o ../obj/$(PLAT)/ibrd1.o ../obj/$(PLAT)/sbrd1.o ../obj/$(PLAT)/cbrd1.o ../obj/$(PLAT)/dbrd1.o ../obj/$(PLAT)/zbrd1.o ../obj/$(PLAT)/ibwr1.o ../obj/$(PLAT)/sbwr1.o ../obj/$(PLAT)/cbwr1.o ../obj/$(PLAT)/dbwr1.o ../obj/$(PLAT)/zbwr1.o ../obj/$(PLAT)/sbrd2.o ../obj/$(PLAT)/ibrd2.o ../obj/$(PLAT)/cbrd2.o ../obj/$(PLAT)/dbrd2.o ../obj/$(PLAT)/zbrd2.o ../obj/$(PLAT)/ibwr2.o ../obj/$(PLAT)/sbwr2.o ../obj/$(PLAT)/cbwr2.o ../obj/$(PLAT)/dbwr2.o ../obj/$(PLAT)/zbwr2.o
OBJS_BLKSVD=../obj/$(PLAT)/sbrdg.o ../obj/$(PLAT)/cbrdg.o ../obj/$(PLAT)/dbrdg.o ../obj/$(PLAT)/zbrdg.o ../obj/$(PLAT)/sbordg.o ../obj/$(PLAT)/cbordg.o ../obj/$(PLAT)/dbordg.o ../obj/$(PLAT)/zbordg.o ../obj/$(PLAT)/nb2m.o
OBJS_DYNORD=../obj/$(PLAT)/dsoffsq.o ../obj/$(PLAT)/dcoffsq.o ../obj/$(PLAT)/spqcmp.o ../obj/$(PLAT)/spqmrg.o ../obj/$(PLAT)/spqsrt.o ../obj/$(PLAT)/spqsort.o ../obj/$(PLAT)/dpqcmp.o ../obj/$(PLAT)/dpqmrg.o ../obj/$(PLAT)/dpqsrt.o ../obj/$(PLAT)/dpqsort.o ../obj/$(PLAT)/sosum4.o ../obj/$(PLAT)/dosum4.o ../obj/$(PLAT)/spsum4.o ../obj/$(PLAT)/dpsum4.o
OBJS_KSVD2=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/cksvd2.o ../obj/$(PLAT)/dksvd2.o ../obj/$(PLAT)/zksvd2.o ../obj/$(PLAT)/slwsv2.o ../obj/$(PLAT)/dlwsv2.o
OBJS_FAUX=../obj/$(PLAT)/slango.o ../obj/$(PLAT)/clango.o ../obj/$(PLAT)/dlango.o ../obj/$(PLAT)/zlango.o ../obj/$(PLAT)/sthalt.o
EXES_DYNORD=../bin/$(PLAT)/spqsort.exe ../bin/$(PLAT)/dpqsort.exe
EXES_KSVD2=../bin/$(PLAT)/sksvd2.exe ../bin/$(PLAT)/cksvd2.exe ../bin/$(PLAT)/dksvd2.exe ../bin/$(PLAT)/zksvd2.exe ../bin/$(PLAT)/slwsv2.exe ../bin/$(PLAT)/dlwsv2.exe ../bin/$(PLAT)/srnd.exe
ifneq ($(COMPILER),nvfortran)
OBJS_ARRBIO += ../obj/$(PLAT)/qbrd1.o ../obj/$(PLAT)/qbwr1.o ../obj/$(PLAT)/qbrd2.o ../obj/$(PLAT)/qbwr2.o
OBJS_DYNORD += ../obj/$(PLAT)/qdoffsq.o ../obj/$(PLAT)/qzoffsq.o ../obj/$(PLAT)/qpqcmp.o ../obj/$(PLAT)/qpqmrg.o ../obj/$(PLAT)/qpqsrt.o ../obj/$(PLAT)/qpqsort.o ../obj/$(PLAT)/qpsum4.o
OBJS_KSVD2 += ../obj/$(PLAT)/qksvd2.o ../obj/$(PLAT)/yksvd2.o
EXES_DYNORD += ../bin/$(PLAT)/qpqsort.exe
EXES_KSVD2 += ../bin/$(PLAT)/qksvd2.exe ../bin/$(PLAT)/yksvd2.exe
ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJS_ARRBIO += ../obj/$(PLAT)/xbrd1.o ../obj/$(PLAT)/xbwr1.o ../obj/$(PLAT)/xbrd2.o ../obj/$(PLAT)/xbwr2.o
OBJS_DYNORD += ../obj/$(PLAT)/xdoffsq.o ../obj/$(PLAT)/xzoffsq.o ../obj/$(PLAT)/qxoffsq.o ../obj/$(PLAT)/qwoffsq.o ../obj/$(PLAT)/xpqcmp.o ../obj/$(PLAT)/xpqmrg.o ../obj/$(PLAT)/xpqsrt.o ../obj/$(PLAT)/xpqsort.o ../obj/$(PLAT)/xpsum4.o
OBJS_KSVD2 += ../obj/$(PLAT)/xksvd2.o ../obj/$(PLAT)/wksvd2.o
EXES_DYNORD += ../bin/$(PLAT)/xpqsort.exe
EXES_KSVD2 += ../bin/$(PLAT)/xksvd2.exe ../bin/$(PLAT)/wksvd2.exe
endif # x86
endif # gfortran
endif # !nvfortran
ifeq ($(findstring DUSE_IEEE_INTRINSIC,$(FCFLAGS)),DUSE_IEEE_INTRINSIC)
INTRIN=i
endif # DUSE_IEEE_INTRINSIC
OBJS=$(OBJS_ARRBIO) $(OBJS_BLKSVD) $(OBJS_DYNORD) $(OBJS_KSVD2) $(OBJS_FAUX)
LIBS=../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a ../lib/$(PLAT)/libfaux.a
EXES=$(EXES_KSVD2) $(EXES_DYNORD)

.PHONY: all clean dirs doc help purge

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=ilp64|lp64] [NDEBUG=optimization_level] [CR_MATH=dir] [all|clean|dirs|doc|help|purge]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

doc: ../Doxyfile
	cd .. && doxygen && cd src

../lib/$(PLAT)/libarrbio.a: $(OBJS_ARRBIO) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_ARRBIO)

../lib/$(PLAT)/libblksvd.a: $(OBJS_BLKSVD) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_BLKSVD)

../lib/$(PLAT)/libdynord.a: $(OBJS_DYNORD) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_DYNORD)

../lib/$(PLAT)/libksvd2.a: $(OBJS_KSVD2) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_KSVD2)

../lib/$(PLAT)/libfaux.a: $(OBJS_FAUX) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS_FAUX)

../obj/$(PLAT)/sksvd2.o: sksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sksvd2.F90

../obj/$(PLAT)/cksvd2.o: cksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cksvd2.F90

../obj/$(PLAT)/dksvd2.o: dksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dksvd2.F90

../obj/$(PLAT)/zksvd2.o: zksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zksvd2.F90

../obj/$(PLAT)/qksvd2.o: qksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qksvd2.F90

../obj/$(PLAT)/yksvd2.o: yksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ yksvd2.F90

../obj/$(PLAT)/xksvd2.o: xksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xksvd2.F90

../obj/$(PLAT)/wksvd2.o: wksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ wksvd2.F90

../obj/$(PLAT)/slwsv2.o: slwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ slwsv2.f90

../obj/$(PLAT)/dlwsv2.o: dlwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dlwsv2.f90

../obj/$(PLAT)/sbrdg.o: sbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbrdg.f90

../obj/$(PLAT)/cbrdg.o: cbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbrdg.f90

../obj/$(PLAT)/dbrdg.o: dbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbrdg.f90

../obj/$(PLAT)/zbrdg.o: zbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbrdg.f90

../obj/$(PLAT)/sbordg.o: sbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbordg.f90

../obj/$(PLAT)/cbordg.o: cbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbordg.f90

../obj/$(PLAT)/dbordg.o: dbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbordg.f90

../obj/$(PLAT)/zbordg.o: zbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbordg.f90

../obj/$(PLAT)/nb2m.o: nb2m.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ nb2m.f90

../obj/$(PLAT)/dsoffsq.o: dsoffsq.F90 goffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dsoffsq.F90

../obj/$(PLAT)/dcoffsq.o: dcoffsq.F90 hoffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dcoffsq.F90

../obj/$(PLAT)/qdoffsq.o: qdoffsq.F90 goffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qdoffsq.F90

../obj/$(PLAT)/qzoffsq.o: qzoffsq.F90 hoffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qzoffsq.F90

../obj/$(PLAT)/xdoffsq.o: xdoffsq.F90 goffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xdoffsq.F90

../obj/$(PLAT)/xzoffsq.o: xzoffsq.F90 hoffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xzoffsq.F90

../obj/$(PLAT)/qxoffsq.o: qxoffsq.F90 goffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qxoffsq.F90

../obj/$(PLAT)/qwoffsq.o: qwoffsq.F90 hoffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qwoffsq.F90

../obj/$(PLAT)/spqcmp.o: spqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ spqcmp.f90

../obj/$(PLAT)/dpqcmp.o: dpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqcmp.f90

../obj/$(PLAT)/qpqcmp.o: qpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqcmp.f90

../obj/$(PLAT)/xpqcmp.o: xpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqcmp.f90

../obj/$(PLAT)/spqmrg.o: spqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ spqmrg.f90

../obj/$(PLAT)/dpqmrg.o: dpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqmrg.f90

../obj/$(PLAT)/qpqmrg.o: qpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqmrg.f90

../obj/$(PLAT)/xpqmrg.o: xpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqmrg.f90

../obj/$(PLAT)/spqsrt.o: spqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ spqsrt.F90

../obj/$(PLAT)/dpqsrt.o: dpqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqsrt.F90

../obj/$(PLAT)/spqsort.o: spqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ spqsort.F90

../obj/$(PLAT)/dpqsort.o: dpqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqsort.F90

../obj/$(PLAT)/qpqsrt.o: qpqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqsrt.F90

../obj/$(PLAT)/qpqsort.o: qpqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqsort.F90

../obj/$(PLAT)/xpqsrt.o: xpqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqsrt.F90

../obj/$(PLAT)/xpqsort.o: xpqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqsort.F90

../obj/$(PLAT)/sosum4.o: sosum4.F90 gosum4.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sosum4.F90

../obj/$(PLAT)/dosum4.o: dosum4.F90 gosum4.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dosum4.F90

../obj/$(PLAT)/spsum4.o: spsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ spsum4.f90

../obj/$(PLAT)/dpsum4.o: dpsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpsum4.f90

../obj/$(PLAT)/qpsum4.o: qpsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpsum4.f90

../obj/$(PLAT)/xpsum4.o: xpsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpsum4.f90

../obj/$(PLAT)/bfopen.o: bfopen.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ bfopen.f90

../obj/$(PLAT)/ibrd1.o: ibrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ ibrd1.f90

../obj/$(PLAT)/sbrd1.o: sbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbrd1.f90

../obj/$(PLAT)/cbrd1.o: cbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbrd1.f90

../obj/$(PLAT)/dbrd1.o: dbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbrd1.f90

../obj/$(PLAT)/zbrd1.o: zbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbrd1.f90

../obj/$(PLAT)/qbrd1.o: qbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qbrd1.f90

../obj/$(PLAT)/xbrd1.o: xbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xbrd1.f90

../obj/$(PLAT)/ibwr1.o: ibwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ ibwr1.f90

../obj/$(PLAT)/sbwr1.o: sbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbwr1.f90

../obj/$(PLAT)/cbwr1.o: cbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbwr1.f90

../obj/$(PLAT)/dbwr1.o: dbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbwr1.f90

../obj/$(PLAT)/zbwr1.o: zbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbwr1.f90

../obj/$(PLAT)/qbwr1.o: qbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qbwr1.f90

../obj/$(PLAT)/xbwr1.o: xbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xbwr1.f90

../obj/$(PLAT)/ibrd2.o: ibrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ ibrd2.f90

../obj/$(PLAT)/sbrd2.o: sbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbrd2.f90

../obj/$(PLAT)/cbrd2.o: cbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbrd2.f90

../obj/$(PLAT)/dbrd2.o: dbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbrd2.f90

../obj/$(PLAT)/zbrd2.o: zbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbrd2.f90

../obj/$(PLAT)/qbrd2.o: qbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qbrd2.f90

../obj/$(PLAT)/xbrd2.o: xbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xbrd2.f90

../obj/$(PLAT)/ibwr2.o: ibwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ ibwr2.f90

../obj/$(PLAT)/sbwr2.o: sbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbwr2.f90

../obj/$(PLAT)/cbwr2.o: cbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbwr2.f90

../obj/$(PLAT)/dbwr2.o: dbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbwr2.f90

../obj/$(PLAT)/zbwr2.o: zbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbwr2.f90

../obj/$(PLAT)/qbwr2.o: qbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qbwr2.f90

../obj/$(PLAT)/xbwr2.o: xbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xbwr2.f90

../obj/$(PLAT)/slango.o: slango.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ slango.f90

../obj/$(PLAT)/clango.o: clango.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ clango.f90

../obj/$(PLAT)/dlango.o: dlango.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dlango.f90

../obj/$(PLAT)/zlango.o: zlango.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zlango.f90

../obj/$(PLAT)/sthalt.o: sthalt.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sthalt.F90

../bin/$(PLAT)/sksvd2.exe: sksvd2x.F90 tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ sksvd2x.F90 $(OBJS_CRF_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/cksvd2.exe: cksvd2x.F90 uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ cksvd2x.F90 $(OBJS_CRF_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/dksvd2.exe: dksvd2x.F90 tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dksvd2x.F90 $(OBJS_CRD_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/zksvd2.exe: zksvd2x.F90 uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ zksvd2x.F90 $(OBJS_CRD_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/qksvd2.exe: qksvd2x.F90 tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ qksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/yksvd2.exe: yksvd2x.F90 uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ yksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/xksvd2.exe: xksvd2x.F90 tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ xksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/wksvd2.exe: wksvd2x.F90 uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ wksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/slwsv2.exe: slwsv2x.F90 tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ slwsv2x.F90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/dlwsv2.exe: dlwsv2x.F90 tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dlwsv2x.F90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/srnd.exe: srnd.f90 $(MKFS)
	$(FC) $(FCFLAGS) -o $@ srnd.f90

../bin/$(PLAT)/spqsort.exe: spqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ spqsortx.f90 -L../lib/$(PLAT) -ldynord

../bin/$(PLAT)/dpqsort.exe: dpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dpqsortx.f90 -L../lib/$(PLAT) -ldynord

../bin/$(PLAT)/qpqsort.exe: qpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ qpqsortx.f90 -L../lib/$(PLAT) -ldynord

../bin/$(PLAT)/xpqsort.exe: xpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ xpqsortx.f90 -L../lib/$(PLAT) -ldynord

clean:
	-$(DEL) $(EXES) $(LIBS) $(OBJS)

purge:
	-$(DEL) ../bin ../lib ../obj ../doc
