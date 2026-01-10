include system.mk
AR=ar
ARFLAGS=rsv
ifndef LIBPVN
LIBPVN=../../libpvn
endif # LIBPVN
include $(LIBPVN)/src/pvn.mk
FC=$(PVN_FC)
ifeq ($(findstring ifx,$(FC)),ifx)
COMPILER=ifx
else # gfortran
COMPILER=gf
endif # ifx
include debug.mk
MKFS=system.mk debug.mk $(LIBPVN)/src/pvn.mk
ifeq ($(ABI),lp64)
FCFLAGS=
else # ilp64
include $(ABI).mk
MKFS += $(ABI).mk
endif # ?lp64
ifeq ($(ARCH),ppc64le)
CPPFLAGS += -DCLS=128
endif # ppc64le
ifdef LAPACK
CPPFLAGS += -DLAPACK=$(LAPACK)
ifdef MKLROOT
ifeq ($(ABI),ilp64)
CPPFLAGS += -DMKL_ILP64=$(LAPACK)
else # lp64
CPPFLAGS += -DMKL_LP64=$(LAPACK)
endif # ?ABI
ifeq ($(OS),Darwin)
MKLFLAGS=${MKLROOT}/lib/libmkl_intel_$(ABI).a ${MKLROOT}/lib/libmkl_$(LAPACK).a ${MKLROOT}/lib/libmkl_core.a
else # Linux
MKLFLAGS=-Wl,--start-group ${MKLROOT}/lib/libmkl_$(COMPILER)_$(ABI).a ${MKLROOT}/lib/libmkl_$(LAPACK).a ${MKLROOT}/lib/libmkl_core.a -Wl,--end-group
ifeq ($(COMPILER),ifx)
MKLFLAGS += ${ONEAPI_ROOT}/compiler/latest/lib/libifcoremt_pic.a
endif # ifx
MKLFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # ?Darwin
else # !MKLROOT
MKLFLAGS=-L$(LAPACK) -llapack -lrefblas
endif # ?MKLROOT
endif # LAPACK
ifeq ($(COMPILER),gf)
CPPFLAGS += $(shell if [ `$(FC) -dumpversion | cut -f1 -d.` -ge 15 ]; then echo '-DHAVE_UNSIGNED'; fi)
FCFLAGS += $(shell if [ `$(FC) -dumpversion | cut -f1 -d.` -ge 15 ]; then echo '-funsigned'; fi)
endif # gfortran
CPPFLAGS += $(PVN_CPPFLAGS)
FCFLAGS += $(PVN_FCFLAGS) $(CPPFLAGS)
FLFLAGS=$(subst PIC,PIE,$(FCFLAGS))
LDFLAGS=$(MKLFLAGS) $(PVN_LIBS)
ifndef PLAT
PLAT=$(COMPILER)/$(OS)-$(ARCH)-$(ABI)$(DEBUG)
endif # !PLAT
include o.mk
MKFS += o.mk GNUmakefile arrbio/GNUmakefile blksvd/GNUmakefile dynord/GNUmakefile ksvd2/GNUmakefile faux/GNUmakefile test/GNUmakefile

.PHONY: all asm clean dirs doc help

all: dirs $(LIBS) $(EXES)
	@echo $(CFLG) $(PLAT)

asm: dirs $(OBJS)
	@echo $(CFLG) $(PLAT)

help:
	@echo $(MAKE) "[LAPACK=sequential|...] [PRINTOUT=ERROR_UNIT|OUTPUT_UNIT] [PRINT0UT=ERROR_UNIT|OUTPUT_UNIT] [ANIMATE=ppe] [all|asm|clean|dirs|doc|help]"

dirs:
	@$(MKD) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)

doc: ../Doxyfile
	cd .. && $(DEL) doc && doxygen

include arrbio/GNUmakefile
include blksvd/GNUmakefile
include dynord/GNUmakefile
include ksvd2/GNUmakefile
include faux/GNUmakefile
include test/GNUmakefile

clean:
	-$(DEL) ../bin/$(PLAT) ../lib/$(PLAT) ../obj/$(PLAT)
