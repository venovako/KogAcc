$(warning Flang is NOT supported, use it at your own risk!)
AR=ar
ARFLAGS=rsv
FC=flang-new
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # DEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=native
endif # !MARCH
FCFLAGS=-cpp -march=$(MARCH) -fPIC -fno-omit-frame-pointer -ffp-contract=fast -fhonor-infinities -fhonor-nans -fimplicit-none -fopenmp
ifeq ($(ARCH),Darwin)
FCFLAGS += -fintegrated-as
endif # Darwin
LDFLAGS=-rdynamic -L../../../../libpvn/src -lpvn -ldl
GFC=gfortran
ifdef NDEBUG
GFCFLAGS=-O$(NDEBUG)
else # !NDEBUG
GFCFLAGS=-Og -ggdb3
endif # ?NDEBUG
GFCFLAGS += -march=native -frecursive -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays #-funsigned
GFCFLAGS += $(shell if [ `$(GFC) -dumpversion | cut -f1 -d.` -ge 15 ]; then echo '-funsigned -DHAVE_UNSIGNED'; fi)
ifdef NDEBUG
GFCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
GFCFLAGS += -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
GFCFLAGS += -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type #-pedantic
