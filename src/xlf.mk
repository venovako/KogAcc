AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)xlf2008_r$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0
endif # ?NDEBUG
# with XLF 17.1.1.0 the OpenMP parts fail to compile
#FCFLAGS += -qinit=f90ptr -qsclk=micro -qpic -qsaveopt -qarch=auto -qsimd=auto -qnosave -qstacktemp=-1 -qstrict -qstrict_induction
# XLF 16.1.1.3 does not recognize pwr10
FCFLAGS += -qinit=f90ptr -qsclk=micro -qpic -qsaveopt -qarch=pwr9 -qsimd=auto -qsmp=omp -qstacktemp=-1 -qstrict -qstrict_induction
ifndef NDEBUG
FCFLAGS += -qcheck -qdbg
endif # !NDEBUG
