ifeq ($(COMPILER),gfortran)
FCFLAGS += -march=native
endif # gfortran
ifdef CR_MATH
FCFLAGS += -DCR_MATH
OBJS_CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf.o
OBJS_CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot.o
endif # CR_MATH
