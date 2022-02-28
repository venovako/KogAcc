AR=ar
ARFLAGS=rsv
FC=gfortran
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-Og -g
endif # ?NDEBUG
FCFLAGS += -march=native -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
FCFLAGS += -fgcse-las -fgcse-sm -fipa-pta -ftree-loop-distribution -ftree-loop-im -ftree-loop-ivcanon -fivopts -fvect-cost-model=unlimited -fvariable-expansion-in-unroller -fopt-info-optimized-vec
else # !NDEBUG
FCFLAGS += -fcheck=array-temps -finit-local-zero -finit-real=snan -finit-derived -Wno-compare-reals -Warray-temporaries -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra
ifeq ($(ARCH),Darwin)
FCFLAGS += -Wa,-q
endif # Darwin
