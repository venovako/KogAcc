# OpenMP and REAL128 not fully supported yet
AR=ar
ARFLAGS=rsv
# XLF 17.1.1.2
FC=$(COMPILER_PREFIX)xlf2008_r$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0
endif # ?NDEBUG
FCFLAGS += -qinit=f90ptr -qsclk=micro -qpic -qarch=auto -qextname -qsimd=auto -qstacktemp=-1 -qstrict -qstrict_induction #-qsmp=omp
ifndef NDEBUG
FCFLAGS += -qcheck -qdbg
endif # !NDEBUG
FCFLAGS += -Wl,-E # --export-dynamic
