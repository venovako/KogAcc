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
ifndef PROFILE
FCFLAGS += -fopenmp
else # PROFILE
ifeq ($(PROFILE),0)
FCFLAGS += -frecursive
else # PROFILE != 0
FCFLAGS += -fopenmp
endif # PROFILE ?= 0
endif # ?PROFILE
FCFLAGS += -fPIC -fexceptions -fasynchronous-unwind-tables -fno-omit-frame-pointer -ffp-contract=fast -ffree-line-length-none -fstack-arrays
ifdef NDEBUG
FCFLAGS += -fno-math-errno -fvect-cost-model=unlimited
else # !NDEBUG
FCFLAGS += -fcheck=all,no-recursion -finit-local-zero -finit-real=snan -finit-derived -Wcharacter-truncation -Wimplicit-procedure -Wfunction-elimination -Wrealloc-lhs-all
endif # ?NDEBUG
FCFLAGS += -Wall -Wextra -Wno-array-temporaries -Wno-compare-reals -Wno-c-binding-type #-pedantic
ifdef STATIC
LDFLAGS=-static
ifneq ($(STATIC),true)
# e.g., STATIC=-s
LDFLAGS += $(STATIC)
endif # !true
else # !STATIC
LDFLAGS=-rdynamic -static-libgcc -static-libgfortran -static-libquadmath #-pie
endif # ?STATIC
ifdef LAPACK
FCFLAGS += -DLAPACK=$(LAPACK)
ifdef MKLROOT
ifeq ($(ABI),ilp64)
FCFLAGS += -DMKL_ILP64=$(LAPACK)
else # lp64
FCFLAGS += -DMKL_LP64=$(LAPACK)
endif # ?ABI
ifeq ($(OS),Darwin)
LDFLAGS += ${MKLROOT}/lib/libmkl_intel_$(ABI).a ${MKLROOT}/lib/libmkl_$(LAPACK).a ${MKLROOT}/lib/libmkl_core.a
ifeq ($(LAPACK),intel_thread)
LDFLAGS += ${CMPLR_ROOT}/mac/compiler/lib/libiomp5.a
endif # intel_thread
else # Linux
LDFLAGS += -Wl,--start-group ${MKLROOT}/lib/libmkl_gf_$(ABI).a ${MKLROOT}/lib/libmkl_$(LAPACK).a ${MKLROOT}/lib/libmkl_core.a -Wl,--end-group
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # ?Darwin
else # !MKLROOT
LDFLAGS += -L$(LAPACK) -llapack -lrefblas #-ltmglib
endif # ?MKLROOT
endif # LAPACK
LDFLAGS += -L../../../../libpvn/src -lpvn -ldl
GFC=$(FC)
GFCFLAGS=$(FCFLAGS)
