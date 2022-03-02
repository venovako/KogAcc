include system.mk
include $(COMPILER).mk
include $(ARCH).mk
include $(ABI).mk
include $(OS).mk
include debug.mk
MKFS += GNUmakefile system.mk $(COMPILER).mk $(ARCH).mk $(ABI).mk $(OS).mk debug.mk
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk
ifndef PLAT
PLAT=$(COMPILER)/$(OS)-$(ARCH)-$(ABI)$(DEBUG)
endif # !PLAT
OBJ=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/dksvd2.o
EXE=../bin/$(PLAT)/sksvd2.exe ../bin/$(PLAT)/dksvd2.exe
ifneq ($(COMPILER),nvfortran)
OBJ += ../obj/$(PLAT)/qksvd2.o
EXE += ../bin/$(PLAT)/qksvd2.exe
endif # !nvfortran
ifeq ($(COMPILER),gfortran)
ifeq ($(ARCH),x86_64)
OBJ += ../obj/$(PLAT)/xksvd2.o
EXE += ../bin/$(PLAT)/xksvd2.exe
endif # x86_64
endif # gfortran
LIB=../lib/$(PLAT)/libksvd2.a

.PHONY: all clean dirs doc help purge

all: dirs $(LIB) $(EXE)
	@echo $(PLAT) "build ended."

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx|nvfortran] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

doc: ../Doxyfile
	pushd .. && doxygen && popd

$(LIB): $(OBJ) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJ)

../obj/$(PLAT)/sksvd2.o: sksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dksvd2.o: dksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qksvd2.o: qksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/xksvd2.o: xksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xksvd2.f90

../bin/$(PLAT)/sksvd2.exe: sksvd2x.f90 tksvd2.f90 $(LIB) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ sksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/dksvd2.exe: dksvd2x.f90 tksvd2.f90 $(LIB) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ dksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/qksvd2.exe: qksvd2x.f90 tksvd2.f90 $(LIB) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ qksvd2x.f90 -L../lib/$(PLAT) -lksvd2
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qksvd2x.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../bin/$(PLAT)/xksvd2.exe: xksvd2x.f90 tksvd2.f90 $(LIB) $(MKFS)
	$(FC) $(FCFLAGS) -o $@ xksvd2x.f90 -L../lib/$(PLAT) -lksvd2

clean:
	-$(DEL) $(EXE) $(LIB) $(OBJ)

purge:
	-$(DEL) ../bin ../lib ../obj
