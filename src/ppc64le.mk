ifeq ($(COMPILER),gfortran)
FCFLAGS += -mcpu=native
endif # gfortran
