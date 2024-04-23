AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -ggdb3
endif # ?NDEBUG
ifeq ($(ARCH),ppc64le)
FCFLAGS += -mcpu=native -mtraceback=full
else # !ppc64le
FCFLAGS += -march=native
endif # ?ppc64le
FCFLAGS += -fopenmp -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays -rdynamic
ifdef NDEBUG
FCFLAGS += -fvect-cost-model=unlimited -fopt-info-optimized-vec
else # !NDEBUG
FCFLAGS += -fcheck=all,no-recursion -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
ifndef INTRIN
# gfortran supports IEEE_FMA from the version 13 onwards
ifeq ($(shell if [ `$(FC) -dumpversion | cut -f1 -d.` -ge 13 ]; then echo i; fi),i)
INTRIN=85
else # GCC < 13
ifeq ($(findstring 86,$(ARCH)),86)
INTRIN=42
else # !x86
# TODO: on non-Intel architectures, assumes that REAL128 == long double, what might not be true for older machines!
INTRIN=138
endif # ?x86
endif # ?GCC
endif # !INTRIN
ifdef INTRIN
FCFLAGS += -DUSE_IEEE_INTRINSIC=$(INTRIN)
endif # INTRIN
