# REAL128 not fully supported yet
AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)nvfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=native
endif # !MARCH
FCFLAGS += -m64 -mp -KPIC -Mdclchk -Mframe -Meh_frame -Minfo -Mlarge_arrays -Mrecursive -Mstack_arrays -tp=$(MARCH) -Kieee -Mfma -Mnodaz -Mnoflushz -Mnofpapprox -Mnofprelaxed -Mno-recip-div -nvmalloc -traceback
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g -Mbounds -Mchkstk
endif # ?NDEBUG
FCFLAGS += -Wl,-E
