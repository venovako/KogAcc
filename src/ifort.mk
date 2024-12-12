$(warning Intel's ifort has been DEPRECATED, use ifx instead!)
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifort$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=Host
# COMMON-AVX512 for KNLs
endif # !MARCH
ifeq ($(ABI),ilp64)
FCFLAGS += -qmkl-$(ABI)=sequential
else # !ilp64
FCFLAGS += -qmkl=sequential
endif # ?ilp64
FCFLAGS += -diag-disable=10397,10448,10441 -x$(MARCH) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -qopt-multi-version-aggressive -qopt-zmm-usage=high -fp-model precise -fma -fprotect-parens -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -qopenmp -qsimd-honor-fp-model -qsimd-serialize-fp-reduction -standard-semantics -traceback -vec-threshold0 -rdynamic -static-intel
ifdef NDEBUG
FCFLAGS += -fno-math-errno -qopt-report=5
ifndef PROFILE
FCFLAGS += -inline-level=2
endif # !PROFILE
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all -fp-stack-check
endif # ?NDEBUG
ifeq ($(OS),Linux)
ifndef NDEBUG
FCFLAGS += -debug parallel
endif # !NDEBUG
FCFLAGS += -static-libgcc
endif # Linux
