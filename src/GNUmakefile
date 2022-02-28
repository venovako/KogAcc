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
SRC=sksvd2.f90 dksvd2.f90
OBJ=../obj/$(PLAT)/sksvd2.o ../obj/$(PLAT)/dksvd2.o ../obj/$(PLAT)/qksvd2.o
ifeq ($(COMPILER),gfortran)
ifeq ($(ARCH),x86_64)
SRC += xksvd2.f90
OBJ += ../obj/$(PLAT)/xksvd2.o
endif # x86_64
endif # gfortran
LIB=../lib/$(PLAT)/libksvd2.a

.PHONY: all help dirs clean purge

all: dirs $(LIB)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx|nvfortran] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|help|clean|purge]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

$(LIB): $(OBJ) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJ)

../obj/$(PLAT)/sksvd2.o: sksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ sksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) sksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/dksvd2.o: dksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ dksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) dksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/qksvd2.o: qksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ qksvd2.f90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) qksvd2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/xksvd2.o: xksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ xksvd2.f90

clean:
	-$(DEL) $(EXE) $(LIB) $(OBJ)

purge:
	-$(DEL) ../bin ../lib ../obj
