ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8
else # !gfortran
ifeq ($(COMPILER),xlf)
FCFLAGS += -qintsize=8
else # ifort
FCFLAGS += -i8
endif # ?xlf
endif # ?gfortran
