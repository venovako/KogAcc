$(warning GNU Fortran is NOT supported, use it at your own risk!)
AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -ggdb3
endif # ?NDEBUG
ifndef MARCH
MARCH=native
endif # !MARCH
ifeq ($(ARCH),ppc64le)
FCFLAGS += -mcpu=$(MARCH) -mpower8-fusion -mtraceback=full
else # !ppc64le
FCFLAGS += -march=$(MARCH)
endif # ?ppc64le
FCFLAGS += -fopenmp -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
FCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
FCFLAGS += -fcheck=all,no-recursion -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
FCFLAGS += -rdynamic -static-libgcc -static-libgfortran -static-libquadmath
