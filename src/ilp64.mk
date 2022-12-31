ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8 -DUSE_ILP64=$(ABI)
else # !gfortran
ifeq ($(COMPILER),xlf)
FCLAGS += -qintsize=8 -DUSE_ILP64=$(ABI)
else # ifort or ifx
FCFLAGS += -i8 -DUSE_ILP64=$(ABI)
endif # ?xlf
endif # ?gfortran
