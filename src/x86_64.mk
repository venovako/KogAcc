ifeq ($(COMPILER),gfortran)
FCFLAGS += -march=native
endif # gfortran
