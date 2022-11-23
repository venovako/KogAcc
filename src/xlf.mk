AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)xlf2008_r$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0
endif # ?NDEBUG
FCFLAGS += -qinit=f90ptr -qsclk=micro -qpic -qsaveopt -qarch=auto -qsimd=auto -qnosave -qstacktemp=-1 -qstrict -qstrict_induction #-qsmp=omp
ifndef NDEBUG
FCFLAGS += -qcheck -qdbg
endif # !NDEBUG
