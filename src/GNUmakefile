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
SRC=SKSVD2.F90 DKSVD2.F90
OBJ=../obj/$(PLAT)/SKSVD2.o ../obj/$(PLAT)/DKSVD2.o
ifdef USE_EXTENDED
SRC += XKSVD2.F90
OBJ += ../obj/$(PLAT)/XKSVD2.o
else # !USE_EXTENDED
ifneq ($(COMPILER),nvfortran)
SRC += QKSVD2.F90
OBJ += ../obj/$(PLAT)/QKSVD2.o
endif # !nvfortran
endif # ?USE_EXTENDED
LIB=../lib/$(PLAT)/libksvd2.a

.PHONY: all help dirs clean purge

all: dirs $(LIB)

help:
	@echo $(MAKE)

dirs:
	-$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

$(LIB): $(OBJ) $(MKFS)
	$(AR) $(ARFLAGS) $@ $(OBJ)

../obj/$(PLAT)/SKSVD2.o: SKSVD2.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ SKSVD2.F90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) SKSVD2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/DKSVD2.o: DKSVD2.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ DKSVD2.F90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) DKSVD2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/XKSVD2.o: XKSVD2.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ XKSVD2.F90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) XKSVD2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

../obj/$(PLAT)/QKSVD2.o: QKSVD2.F90 $(MKFS)
	$(FC) $(FCFLAGS) -c -o $@ QKSVD2.F90
ifdef NDEBUG
ifeq ($(COMPILER),ifx)
	-$(MOV) QKSVD2.opt.yaml ../obj/$(PLAT)
endif # ifx
endif # NDEBUG

clean:
	-$(DEL) $(EXE) $(LIB) $(OBJ)

purge:
	-$(DEL) ../bin ../lib ../obj
