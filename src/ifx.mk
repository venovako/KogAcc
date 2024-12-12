AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
ifndef MARCH
# common-avx512 for KNLs
MARCH=Host
endif # !MARCH
ifeq ($(ABI),ilp64)
FCFLAGS += -qmkl-$(ABI)=sequential
else # !ilp64
FCFLAGS += -qmkl=sequential
endif # ?ilp64
FCFLAGS += -x$(MARCH) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -mprefer-vector-width=512 -qopenmp -standard-semantics -traceback -vec-threshold0 -rdynamic -static-intel -static-libgcc
ifdef NDEBUG
FCFLAGS += -fno-math-errno -qopt-report=3
ifndef PROFILE
FCFLAGS += -inline-level=2
endif # !PROFILE
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all
endif # ?NDEBUG
ifeq ($(OS),Linux)
ifndef NDEBUG
FCFLAGS += -debug parallel
endif # !NDEBUG
endif # Linux
