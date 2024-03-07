ifdef LAPACK
LAPACK_LIBS=-L$(LAPACK) -ltmglib -llapack -lrefblas -lm
else # !LAPACK
ifeq ($(ARCH),x86_64)
ifeq ($(COMPILER),gfortran)
LAPACK_LIBS=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_gf_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
else # Intel compilers
LAPACK_LIBS=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # ?gfortran
else # !x86_64
# TODO
endif # ?x86_64
endif # ?LAPACK
