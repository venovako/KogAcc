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
FCFLAGS += -x$(MARCH) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -mprefer-vector-width=512 -qopenmp -standard-semantics -traceback -vec-threshold0
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
ifdef STATIC
LDFLAGS=-static
ifneq ($(STATIC),true)
LDFLAGS += $(STATIC)
endif # !true
else # !STATIC
LDFLAGS=-rdynamic -static-intel -static-libgcc #-pie
endif # ?STATIC
ifdef LAPACK
FCFLAGS += -DLAPACK=$(LAPACK)
ifeq ($(ABI),ilp64)
FCFLAGS += -DMKL=2
LDFLAGS += -qmkl-$(ABI)=$(LAPACK)
else # lp64
FCFLAGS += -DMKL=1
LDFLAGS += -qmkl=$(LAPACK)
endif # ?ABI
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # LAPACK
LDFLAGS += -L../../../../libpvn/src -lpvn -ldl $(realpath $(shell gcc -print-file-name=libquadmath.a))
GFC=gfortran
ifdef NDEBUG
GFCFLAGS=-O$(NDEBUG)
else # !NDEBUG
GFCFLAGS=-Og -ggdb3
endif # ?NDEBUG
GFCFLAGS += -march=native -frecursive -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
GFCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
GFCFLAGS += -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
GFCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
