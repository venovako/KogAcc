ifeq ($(COMPILER),gfortran)
FCFLAGS += -fdefault-integer-8
else # ifort or ifx
FCFLAGS += -i8
endif # ?gfortran
