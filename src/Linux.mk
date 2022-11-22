ifndef LAPACK
ifeq ($(ARCH),x86_64)
ifeq ($(COMPILER),gfortran)
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_gf_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
else # other compilers
ifeq ($(COMPILER),nvfortran)
LAPACK=-llapack_$(ABI) -lblas_$(ABI)
else # ifort/ifx
LAPACK=-L$(MKLROOT)/lib/intel64 -Wl,-rpath=$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
endif # ?nvfortran
endif # ?gfortran
else # !x86_64
ifeq ($(COMPILER),nvfortran)
LAPACK=-llapack_$(ABI) -lblas_$(ABI)
else # !nvfortran
LAPACK=-L$(HOME)/lapack_$(ABI) -ltmglib -llapack -lrefblas -lm
endif # ?nvfortran
endif # ?x86_64
endif # !LAPACK
