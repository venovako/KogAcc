include system.mk
include $(COMPILER).mk
include $(OS).mk
include debug.mk
MKFS=system.mk $(COMPILER).mk $(OS).mk debug.mk
ifneq ($(ABI),lp64)
include $(ABI).mk
MKFS += $(ABI).mk
endif # !lp64
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk
MKFS += o.mk GNUmakefile arrbio/GNUmakefile blksvd/GNUmakefile dynord/GNUmakefile ksvd2/GNUmakefile faux/GNUmakefile test/GNUmakefile
ifndef PLAT
PLAT=$(COMPILER)/$(OS)-$(ARCH)-$(ABI)$(DEBUG)
endif # !PLAT
include o.mk

.PHONY: all asm clean dirs doc help

all: dirs $(LIBS) $(EXES)
	@echo $(CFLG) $(PLAT)

asm: dirs $(OBJS)
	@echo $(CFLG) $(PLAT)

help:
	@echo $(MAKE) "[COMPILER=ifx|gfortran] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [INTRIN=...] [CR_MATH=...] [LAPACK=...] [PROFILE=...] [ANIMATE=ppe] [all|asm|clean|dirs|doc|help]"

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
