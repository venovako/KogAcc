include system.mk
include $(COMPILER).mk
include $(ARCH).mk
include $(ABI).mk
include $(OS).mk
include debug.mk
MKFS=GNUmakefile system.mk $(COMPILER).mk $(ARCH).mk $(ABI).mk $(OS).mk debug.mk o.mk
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk
ifndef PLAT
PLAT=$(COMPILER)/$(OS)-$(ARCH)-$(ABI)$(DEBUG)
endif # !PLAT
include o.mk

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
