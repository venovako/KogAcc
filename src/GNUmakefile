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
OBJS_ARRBIO=../obj/$(PLAT)/bfopen.o ../obj/$(PLAT)/ibrd1.o ../obj/$(PLAT)/jbrd1.o ../obj/$(PLAT)/sbrd1.o ../obj/$(PLAT)/cbrd1.o ../obj/$(PLAT)/dbrd1.o ../obj/$(PLAT)/zbrd1.o ../obj/$(PLAT)/ibwr1.o ../obj/$(PLAT)/jbwr1.o ../obj/$(PLAT)/sbwr1.o ../obj/$(PLAT)/cbwr1.o ../obj/$(PLAT)/dbwr1.o ../obj/$(PLAT)/zbwr1.o ../obj/$(PLAT)/ibrd2.o ../obj/$(PLAT)/jbrd2.o ../obj/$(PLAT)/sbrd2.o ../obj/$(PLAT)/cbrd2.o ../obj/$(PLAT)/dbrd2.o ../obj/$(PLAT)/zbrd2.o ../obj/$(PLAT)/ibwr2.o ../obj/$(PLAT)/jbwr2.o ../obj/$(PLAT)/sbwr2.o ../obj/$(PLAT)/cbwr2.o ../obj/$(PLAT)/dbwr2.o ../obj/$(PLAT)/zbwr2.o
OBJS_BLKSVD=../obj/$(PLAT)/sbrdg.o ../obj/$(PLAT)/cbrdg.o ../obj/$(PLAT)/dbrdg.o ../obj/$(PLAT)/zbrdg.o ../obj/$(PLAT)/sbordg.o ../obj/$(PLAT)/cbordg.o ../obj/$(PLAT)/dbordg.o ../obj/$(PLAT)/zbordg.o ../obj/$(PLAT)/nb2m.o
OBJS_DYNORD=../obj/$(PLAT)/spqcmp.o ../obj/$(PLAT)/spqmrg.o ../obj/$(PLAT)/spqsrt.o ../obj/$(PLAT)/spqsort.o ../obj/$(PLAT)/dpqcmp.o ../obj/$(PLAT)/dpqmrg.o ../obj/$(PLAT)/dpqsrt.o ../obj/$(PLAT)/dpqsort.o
OBJS_KSVD2=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/cksvd2.o ../obj/$(PLAT)/dksvd2.o ../obj/$(PLAT)/zksvd2.o ../obj/$(PLAT)/slwsv2.o ../obj/$(PLAT)/dlwsv2.o
OBJS_FAUX=../obj/$(PLAT)/sthalt.o ../obj/$(PLAT)/slango.o ../obj/$(PLAT)/clango.o ../obj/$(PLAT)/dlango.o ../obj/$(PLAT)/zlango.o ../obj/$(PLAT)/sosum4.o ../obj/$(PLAT)/dosum4.o ../obj/$(PLAT)/dsoffsq.o ../obj/$(PLAT)/dcoffsq.o ../obj/$(PLAT)/spsum4.o ../obj/$(PLAT)/dpsum4.o 
EXES_DYNORD=../bin/$(PLAT)/spqsort.exe ../bin/$(PLAT)/dpqsort.exe
EXES_KSVD2=../bin/$(PLAT)/sksvd2.exe ../bin/$(PLAT)/cksvd2.exe ../bin/$(PLAT)/dksvd2.exe ../bin/$(PLAT)/zksvd2.exe ../bin/$(PLAT)/slwsv2.exe ../bin/$(PLAT)/dlwsv2.exe ../bin/$(PLAT)/srnd.exe
ifneq ($(COMPILER),nvfortran)
OBJS_ARRBIO += ../obj/$(PLAT)/qbrd1.o ../obj/$(PLAT)/qbwr1.o ../obj/$(PLAT)/qbrd2.o ../obj/$(PLAT)/qbwr2.o ../obj/$(PLAT)/ybrd1.o ../obj/$(PLAT)/ybwr1.o ../obj/$(PLAT)/ybrd2.o ../obj/$(PLAT)/ybwr2.o
OBJS_DYNORD += ../obj/$(PLAT)/qpqcmp.o ../obj/$(PLAT)/qpqmrg.o ../obj/$(PLAT)/qpqsrt.o ../obj/$(PLAT)/qpqsort.o
OBJS_KSVD2 += ../obj/$(PLAT)/qksvd2.o ../obj/$(PLAT)/yksvd2.o
OBJS_FAUX += ../obj/$(PLAT)/qdoffsq.o ../obj/$(PLAT)/qzoffsq.o ../obj/$(PLAT)/qpsum4.o
EXES_DYNORD += ../bin/$(PLAT)/qpqsort.exe
EXES_KSVD2 += ../bin/$(PLAT)/qksvd2.exe ../bin/$(PLAT)/yksvd2.exe
ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJS_ARRBIO += ../obj/$(PLAT)/xbrd1.o ../obj/$(PLAT)/xbwr1.o ../obj/$(PLAT)/xbrd2.o ../obj/$(PLAT)/xbwr2.o ../obj/$(PLAT)/wbrd1.o ../obj/$(PLAT)/wbwr1.o ../obj/$(PLAT)/wbrd2.o ../obj/$(PLAT)/wbwr2.o
OBJS_DYNORD += ../obj/$(PLAT)/xpqcmp.o ../obj/$(PLAT)/xpqmrg.o ../obj/$(PLAT)/xpqsrt.o ../obj/$(PLAT)/xpqsort.o
OBJS_KSVD2 += ../obj/$(PLAT)/xksvd2.o ../obj/$(PLAT)/wksvd2.o
OBJS_FAUX += ../obj/$(PLAT)/xdoffsq.o ../obj/$(PLAT)/xzoffsq.o ../obj/$(PLAT)/qxoffsq.o ../obj/$(PLAT)/qwoffsq.o ../obj/$(PLAT)/xpsum4.o
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
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx|nvfortran|xlf] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=ilp64|lp64] [NDEBUG=optimization_level] [CR_MATH=dir] [all|clean|dirs|doc|help|purge]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

doc: ../Doxyfile
	cd .. && doxygen && cd src

include arrbio/GNUmakefile
include blksvd/GNUmakefile
include dynord/GNUmakefile
include ksvd2/GNUmakefile
include faux/GNUmakefile
include test/GNUmakefile

clean:
	-$(DEL) $(EXES) $(LIBS) $(OBJS)

purge:
	-$(DEL) ../bin ../lib ../obj ../doc
