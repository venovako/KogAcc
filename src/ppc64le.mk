ifeq ($(COMPILER),gfortran)
FCFLAGS += -mcpu=native -mtraceback=full
endif # gfortran
