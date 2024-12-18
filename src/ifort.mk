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
# COMMON-AVX512 for KNLs
MARCH=Host
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
GFC=gfortran$(GNU)
ifdef NDEBUG
GFCFLAGS=-O$(NDEBUG)
else # !NDEBUG
GFCFLAGS=-Og -ggdb3
endif # ?NDEBUG
ifeq ($(OS),Darwin)
GFCFLAGS += -Wa,-q
endif # Darwin
# -frecursive if not -fopenmp
GFCFLAGS += -march=native -fopenmp -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
GFCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
GFCFLAGS += -fcheck=all,no-recursion -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
GFCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
GFCFLAGS += -rdynamic -static-libgcc -static-libgfortran -static-libquadmath
