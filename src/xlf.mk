AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)xlf2008_r$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0
endif # ?NDEBUG
# XLF 16.1.1.13
FCFLAGS += -qinit=f90ptr -qsclk=micro -qpic -qsaveopt -qarch=auto -qsimd=auto -qsmp=omp -qstacktemp=-1 -qstrict -qstrict_induction
ifndef NDEBUG
FCFLAGS += -qcheck -qdbg
endif # !NDEBUG
# ifdef INTRIN
# FCFLAGS += -DUSE_IEEE_INTRINSIC=$(INTRIN)
# endif # INTRIN
FCFLAGS += -Wl,-E # --export-dynamic
