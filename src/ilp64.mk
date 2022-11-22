ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8
else # !gfortran
ifeq ($(COMPILER),xlf)
FCFLAGS += -qintsize=8
else # ifort, ifx, or nvfortran
FCFLAGS += -i8
endif # ?xlf
endif # ?gfortran
