include system.mk
include $(COMPILER).mk
include $(ARCH).mk
include $(ABI).mk
include $(OS).mk
include debug.mk
MKFS=system.mk $(COMPILER).mk $(ARCH).mk $(ABI).mk $(OS).mk debug.mk
ifneq ($(wildcard local.mk),)
include local.mk
MKFS += local.mk
endif # local.mk
MKFS += o.mk GNUmakefile arrbio/GNUmakefile blksvd/GNUmakefile dynord/GNUmakefile ksvd2/GNUmakefile faux/GNUmakefile test/GNUmakefile
ifndef PLAT
ifdef CR_MATH
PLAT=$(COMPILER)/$(OS)_$(ARCH)_$(ABI)$(DEBUG)
else # !CR_MATH
PLAT=$(COMPILER)/$(OS)-$(ARCH)-$(ABI)$(DEBUG)
endif # ?CR_MATH
endif # !PLAT
include o.mk

.PHONY: all asm clean dirs doc help

all: dirs $(LIBS) $(EXES)
	@echo $(CFLG) $(PLAT)

asm: dirs $(OBJS)
	@echo $(CFLG) $(PLAT)

help:
	@echo $(MAKE) "[COMPILER=gfortran|ifort|ifx|xlf] [COMPILER_PREFIX=...] [COMPILER_SUFFIX=...] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [INTRIN=...] [CR_MATH=dir] [ANIMATE=ppe] [all|asm|clean|dirs|doc|help]"

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
