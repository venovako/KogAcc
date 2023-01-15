AR=ar
ARFLAGS=rsv
# XLF 16.1.1.13
FC=$(COMPILER_PREFIX)xlf2008_r$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0
endif # ?NDEBUG
FCFLAGS += -qinit=f90ptr -qsclk=micro -qpic -qsaveopt -qarch=auto -qsimd=auto -qsmp=omp -qstacktemp=-1 -qstrict -qstrict_induction
ifndef NDEBUG
FCFLAGS += -qcheck -qdbg
endif # !NDEBUG
FCFLAGS += -Wl,-E # --export-dynamic
