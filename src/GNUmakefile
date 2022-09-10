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
OBJS=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/cksvd2.o ../obj/$(PLAT)/dksvd2.o ../obj/$(PLAT)/zksvd2.o ../obj/$(PLAT)/slwsv2.o ../obj/$(PLAT)/dlwsv2.o
EXES=../bin/$(PLAT)/sksvd2.exe ../bin/$(PLAT)/cksvd2.exe ../bin/$(PLAT)/dksvd2.exe ../bin/$(PLAT)/zksvd2.exe ../bin/$(PLAT)/slwsv2.exe ../bin/$(PLAT)/dlwsv2.exe ../bin/$(PLAT)/srnd.exe
ifneq ($(COMPILER),nvfortran)
OBJS += ../obj/$(PLAT)/qksvd2.o ../obj/$(PLAT)/yksvd2.o
EXES += ../bin/$(PLAT)/qksvd2.exe ../bin/$(PLAT)/yksvd2.exe
endif # !nvfortran
ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJS += ../obj/$(PLAT)/xksvd2.o ../obj/$(PLAT)/wksvd2.o
EXES += ../bin/$(PLAT)/xksvd2.exe ../bin/$(PLAT)/wksvd2.exe
endif # x86
endif # gfortran
LIBS=../lib/$(PLAT)/libksvd2.a

.PHONY: all clean dirs doc help purge

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx|nvfortran] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

doc: ../Doxyfile
	cd .. && doxygen && cd src

$(LIBS): $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

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

../bin/$(PLAT)/sksvd2.exe: sksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ sksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/cksvd2.exe: cksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ cksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) cksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/dksvd2.exe: dksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/zksvd2.exe: zksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ zksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) zksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/qksvd2.exe: qksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ qksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/yksvd2.exe: yksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ yksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) yksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/xksvd2.exe: xksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ xksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/wksvd2.exe: wksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ wksvd2x.f90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/slwsv2.exe: slwsv2x.f90 tlasv2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ slwsv2x.f90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) slwsv2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/dlwsv2.exe: dlwsv2x.f90 tlasv2.f90 $(LIBS) $(MKFS)
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
