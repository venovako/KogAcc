ifeq ($(COMPILER),gfortran)
FCFLAGS += -march=native
endif # gfortran
ifdef CR_MATH
FCFLAGS += -DCR_MATH
ifdef NDEBUG
# modified routines that do not reference errno
OBJS_CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf_noerrno.o
OBJS_CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot_noerrno.o
else # !NDEBUG
OBJS_CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf.o
OBJS_CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot.o
endif # ?NDEBUG
endif # CR_MATH
