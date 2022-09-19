ifeq ($(ARCH),x86_64)
ifeq ($(COMPILER),gfortran)
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_gf_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
else # other compilers
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
endif # ?gfortran
else # ppc64le for now
ifeq ($(COMPILER),gfortran)
LAPACK=-L$(HOME)/lapack -ltmglib -llapack -lrefblas -lm
else # nvfortran for now
LAPACK=-llapack_$(ABI) -lblas_$(ABI)
endif # ?gfortran
endif # ?x86_64
