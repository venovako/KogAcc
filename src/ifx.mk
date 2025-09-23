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
ifndef PROFILE
FCFLAGS += -qopenmp
else # PROFILE
ifeq ($(PROFILE),0)
FCFLAGS += -assume recursion
else # PROFILE != 0
FCFLAGS += -qopenmp
endif # PROFILE ?= 0
endif # ?PROFILE
FCFLAGS += -x$(MARCH) -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -mprefer-vector-width=512 -standard-semantics -traceback -vec-threshold0
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
LDFLAGS=-static $(STATIC)
else # !STATIC
LDFLAGS=-rdynamic
endif # ?STATIC
ifdef LAPACK
FCFLAGS += -DLAPACK=$(LAPACK)
ifeq ($(ABI),ilp64)
FCFLAGS += -DMKL_ILP64=$(LAPACK)
LDFLAGS += -qmkl-$(ABI)=$(LAPACK)
else # lp64
FCFLAGS += -DMKL_LP64=$(LAPACK)
LDFLAGS += -qmkl=$(LAPACK)
endif # ?ABI
ifeq ($(LAPACK),sequential)
FCFLAGS += -DMKL_DIRECT_CALL_SEQ
else # parallel
FCFLAGS += -DMKL_DIRECT_CALL
endif # ?LAPACK
FCFLAGS += -I${MKLROOT}/include
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # LAPACK
LDFLAGS += -L../../../../libpvn/src -lpvn -lquadmath -lgcc_s -lpthread -lm -ldl
GFC=gfortran
ifdef NDEBUG
GFCFLAGS=-O$(NDEBUG)
else # !NDEBUG
GFCFLAGS=-Og -ggdb3
endif # ?NDEBUG
GFCFLAGS += -march=native -frecursive -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays
GFCFLAGS += $(shell if [ `$(GFC) -dumpversion | cut -f1 -d.` -ge 15 ]; then echo '-funsigned -DHAVE_UNSIGNED'; fi)
ifdef NDEBUG
GFCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
GFCFLAGS += -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
GFCFLAGS += -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
