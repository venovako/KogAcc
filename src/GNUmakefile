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
OBJS_BLKSVD=../obj/$(PLAT)/sbrdg.o ../obj/$(PLAT)/cbrdg.o ../obj/$(PLAT)/dbrdg.o ../obj/$(PLAT)/zbrdg.o ../obj/$(PLAT)/sbordg.o ../obj/$(PLAT)/cbordg.o ../obj/$(PLAT)/dbordg.o ../obj/$(PLAT)/zbordg.o ../obj/$(PLAT)/nb2m.o
OBJS_DYNORD=../obj/$(PLAT)/dsoffsq.o ../obj/$(PLAT)/dcoffsq.o ../obj/$(PLAT)/dpqcmp.o ../obj/$(PLAT)/dpqmrg.o ../obj/$(PLAT)/dpqsrt.o ../obj/$(PLAT)/dpqsort.o
OBJS_KSVD2=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/cksvd2.o ../obj/$(PLAT)/dksvd2.o ../obj/$(PLAT)/zksvd2.o ../obj/$(PLAT)/slwsv2.o ../obj/$(PLAT)/dlwsv2.o
EXES_DYNORD=../bin/$(PLAT)/dpqsort.exe
EXES_KSVD2=../bin/$(PLAT)/sksvd2.exe ../bin/$(PLAT)/cksvd2.exe ../bin/$(PLAT)/dksvd2.exe ../bin/$(PLAT)/zksvd2.exe ../bin/$(PLAT)/slwsv2.exe ../bin/$(PLAT)/dlwsv2.exe ../bin/$(PLAT)/srnd.exe
# quad
OBJS_ARRBIO += ../obj/$(PLAT)/qbrd1.o ../obj/$(PLAT)/qbwr1.o
OBJS_DYNORD += ../obj/$(PLAT)/qdoffsq.o ../obj/$(PLAT)/qzoffsq.o ../obj/$(PLAT)/qpqcmp.o ../obj/$(PLAT)/qpqmrg.o ../obj/$(PLAT)/qpqsrt.o ../obj/$(PLAT)/qpqsort.o
OBJS_KSVD2 += ../obj/$(PLAT)/qksvd2.o ../obj/$(PLAT)/yksvd2.o
EXES_DYNORD += ../bin/$(PLAT)/qpqsort.exe
EXES_KSVD2 += ../bin/$(PLAT)/qksvd2.exe ../bin/$(PLAT)/yksvd2.exe
ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJS_ARRBIO += ../obj/$(PLAT)/xbrd1.o ../obj/$(PLAT)/xbwr1.o
OBJS_DYNORD += ../obj/$(PLAT)/xdoffsq.o ../obj/$(PLAT)/xzoffsq.o ../obj/$(PLAT)/xpqcmp.o ../obj/$(PLAT)/xpqmrg.o ../obj/$(PLAT)/xpqsrt.o ../obj/$(PLAT)/xpqsort.o
OBJS_KSVD2 += ../obj/$(PLAT)/xksvd2.o ../obj/$(PLAT)/wksvd2.o
EXES_DYNORD += ../bin/$(PLAT)/xpqsort.exe
EXES_KSVD2 += ../bin/$(PLAT)/xksvd2.exe ../bin/$(PLAT)/wksvd2.exe
endif # x86
endif # gfortran
OBJS=$(OBJS_ARRBIO) $(OBJS_BLKSVD) $(OBJS_DYNORD) $(OBJS_KSVD2)
LIBS=../lib/$(PLAT)/libarrbio.a ../lib/$(PLAT)/libblksvd.a ../lib/$(PLAT)/libdynord.a ../lib/$(PLAT)/libksvd2.a
EXES=$(EXES_KSVD2) $(EXES_DYNORD)

.PHONY: all clean dirs doc help purge

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

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

../obj/$(PLAT)/sksvd2.o: sksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sksvd2.f90

../obj/$(PLAT)/cksvd2.o: cksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cksvd2.f90

../obj/$(PLAT)/dksvd2.o: dksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dksvd2.f90

../obj/$(PLAT)/zksvd2.o: zksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zksvd2.f90

../obj/$(PLAT)/qksvd2.o: qksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qksvd2.f90

../obj/$(PLAT)/yksvd2.o: yksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ yksvd2.f90

../obj/$(PLAT)/xksvd2.o: xksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xksvd2.f90

../obj/$(PLAT)/wksvd2.o: wksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ wksvd2.f90

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

../obj/$(PLAT)/dsoffsq.o: dsoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dsoffsq.f90

../obj/$(PLAT)/dcoffsq.o: dcoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dcoffsq.f90

../obj/$(PLAT)/qdoffsq.o: qdoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qdoffsq.f90

../obj/$(PLAT)/qzoffsq.o: qzoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qzoffsq.f90

../obj/$(PLAT)/xdoffsq.o: xdoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xdoffsq.f90

../obj/$(PLAT)/xzoffsq.o: xzoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xzoffsq.f90

../obj/$(PLAT)/dpqcmp.o: dpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqcmp.f90

../obj/$(PLAT)/qpqcmp.o: qpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqcmp.f90

../obj/$(PLAT)/xpqcmp.o: xpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqcmp.f90

../obj/$(PLAT)/dpqmrg.o: dpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqmrg.f90

../obj/$(PLAT)/qpqmrg.o: qpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqmrg.f90

../obj/$(PLAT)/xpqmrg.o: xpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqmrg.f90

../obj/$(PLAT)/dpqsrt.o: dpqsrt.f90 gpqsrt.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqsrt.f90

../obj/$(PLAT)/dpqsort.o: dpqsort.f90 gpqsort.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dpqsort.f90

../obj/$(PLAT)/qpqsrt.o: qpqsrt.f90 gpqsrt.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqsrt.f90

../obj/$(PLAT)/qpqsort.o: qpqsort.f90 gpqsort.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qpqsort.f90

../obj/$(PLAT)/xpqsrt.o: xpqsrt.f90 gpqsrt.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqsrt.f90

../obj/$(PLAT)/xpqsort.o: xpqsort.f90 gpqsort.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xpqsort.f90

../obj/$(PLAT)/bfopen.o: bfopen.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ bfopen.f90

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

../obj/$(PLAT)/sbrd2.o: sbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbrd2.f90

../obj/$(PLAT)/cbrd2.o: cbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbrd2.f90

../obj/$(PLAT)/dbrd2.o: dbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbrd2.f90

../obj/$(PLAT)/zbrd2.o: zbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbrd2.f90

../obj/$(PLAT)/sbwr2.o: sbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sbwr2.f90

../obj/$(PLAT)/cbwr2.o: cbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ cbwr2.f90

../obj/$(PLAT)/dbwr2.o: dbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dbwr2.f90

../obj/$(PLAT)/zbwr2.o: zbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ zbwr2.f90

../bin/$(PLAT)/sksvd2.exe: sksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ sksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/cksvd2.exe: cksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ cksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/dksvd2.exe: dksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/zksvd2.exe: zksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ zksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/qksvd2.exe: qksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ qksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/yksvd2.exe: yksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ yksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/xksvd2.exe: xksvd2x.f90 tksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ xksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/wksvd2.exe: wksvd2x.f90 uksvd2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ wksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/slwsv2.exe: slwsv2x.f90 tlasv2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ slwsv2x.f90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/dlwsv2.exe: dlwsv2x.f90 tlasv2.f90 ../lib/$(PLAT)/libksvd2.a $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dlwsv2x.f90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/srnd.exe: srnd.f90 $(MKFS)
	$(FC) $(FCFLAGS) -o $@ srnd.f90

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
