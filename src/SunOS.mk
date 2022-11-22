ifndef LAPACK
LAPACK=-L$(HOME)/lapack_$(ABI) -ltmglib -llapack -lrefblas -lm
endif # !LAPACK
