ifeq ($(ARCH),x86_64)
ifeq ($(COMPILER),gfortran)
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_gf_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
else # other compilers
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
endif # ?gfortran
else # ppc64le lp64 for now
LAPACK=-llapack -lblas -lm 
endif # ?x86_64
