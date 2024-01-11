ifndef LAPACK
ifeq ($(ARCH),x86_64)
ifeq ($(COMPILER),gfortran)
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_gf_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
else # Intel compilers
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # ?gfortran
else # !x86_64
LAPACK=-L$(HOME)/lapack-$(ABI) -ltmglib -llapack -lrefblas -lm
endif # ?x86_64
endif # !LAPACK
