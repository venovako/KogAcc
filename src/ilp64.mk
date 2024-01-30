ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8
else # !gfortran
ifeq ($(COMPILER),xlf)
FCLAGS += -qintsize=8
else # ifort or ifx
FCFLAGS += -i8
endif # ?xlf
endif # ?gfortran
FCFLAGS += -DMKL_ILP64=$(ABI)
