ifndef LAPACK
LAPACK=-L$(HOME)/lapack-$(ABI) -ltmglib -llapack -lrefblas -lm
endif # !LAPACK
