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
FCFLAGS += -fcheck=all,no-recursion -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
ifndef INTRIN
# gfortran might support IEEE_FMA from the version 13 onwards
ifeq ($(shell if [ `$(FC) -dumpversion | cut -f1 -d.` -ge 13 ]; then echo i; fi),i)
INTRIN=i
endif # i
endif # !INTRIN
ifdef INTRIN
FCFLAGS += -DUSE_IEEE_INTRINSIC=$(INTRIN)
endif # INTRIN
