ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8
else # !gfortran
FCFLAGS += -i8
endif # ?gfortran
