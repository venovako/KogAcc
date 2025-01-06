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
FCFLAGS += -DCLS=128 -mcpu=$(MARCH) -mpower8-fusion -mtraceback=full
else # !ppc64le
FCFLAGS += -march=$(MARCH)
ifeq ($(OS),Darwin)
FCFLAGS += -Wa,-q
endif # Darwin
endif # ?ppc64le
FCFLAGS += -fopenmp -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
FCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
FCFLAGS += -fcheck=all,no-recursion -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -pedantic -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type
ifdef STATIC
LDFLAGS=-static
ifneq ($(STATIC),true)
LDFLAGS += $(STATIC)
endif # !true
else # !STATIC
LDFLAGS=-rdynamic -static-libgcc -static-libgfortran -static-libquadmath #-pie
endif # ?STATIC
# ifdef MKLROOT
# ifeq ($(OS),Darwin)
# LDFLAGS += ${MKLROOT}/lib/libmkl_intel_$(ABI).a ${MKLROOT}/lib/libmkl_sequential.a ${MKLROOT}/lib/libmkl_core.a
# else # Linux
# LDFLAGS += -Wl,--start-group ${MKLROOT}/lib/libmkl_gf_$(ABI).a ${MKLROOT}/lib/libmkl_sequential.a ${MKLROOT}/lib/libmkl_core.a -Wl,--end-group
# LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
# endif # ?Darwin
# else # !MKLROOT
# ifndef LAPACK
# LAPACK=$(HOME)/lapack-$(ABI)
# endif # !LAPACK
# LDFLAGS += -L$(LAPACK) -ltmglib -llapack -lrefblas
# endif # ?MKLROOT
LDFLAGS += -L../../../../libpvn/src -lpvn -ldl
GFC=$(FC)
GFCFLAGS=$(FCFLAGS)
