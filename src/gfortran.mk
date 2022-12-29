AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)gfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -g
endif # ?NDEBUG
FCFLAGS += -fopenmp -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
FCFLAGS += -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller -fopt-info-optimized-vec
else # !NDEBUG
FCFLAGS += -fcheck=all -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals
# makes no sense to use CR_MATH without enforcing the FMAs in the rest of the code, if possible
ifdef CR_MATH
# gfortran might support IEEE_FMA from the version 13 onwards
ifndef INTRIN
ifeq ($(shell if [ `$(FC) -dumpversion | cut -f1 -d.` -ge 13 ]; then echo i; fi),i)
INTRIN=i
endif # i
endif # !INTRIN
endif # CR_MATH
ifdef INTRIN
FCFLAGS += -DUSE_IEEE_INTRINSIC=$(INTRIN)
endif # INTRIN
