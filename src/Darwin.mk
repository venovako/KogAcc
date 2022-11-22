ifndef LAPACK
ifeq ($(ARCH),x86_64)
LAPACK=-L$(MKLROOT)/lib -Wl,-rpath,$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
else # !x86_64
LAPACK=-L$(HOME)/lapack_$(ABI) -ltmglib -llapack -lrefblas -lm
endif # ?x86_64
endif # !LAPACK
