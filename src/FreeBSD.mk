ifdef LAPACK
LAPACK_LIBS=-L$(LAPACK) -ltmglib -llapack -lrefblas -lm
endif # !LAPACK
