AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)nvfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
ifndef CPU
CPU=native
endif # !CPU
FCFLAGS += -m64 -mp -KPIC -Mdclchk -Mframe -Meh_frame -Minfo -Mlarge_arrays -Mrecursive -Mstack_arrays -tp=$(CPU) -Kieee -Mfma -Mnodaz -Mnoflushz -Mnofpapprox -Mnofprelaxed -Mno-recip-div -nvmalloc -traceback
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g -Mbounds -Mchkstk
endif # ?NDEBUG
FCFLAGS += -Wl,-E
