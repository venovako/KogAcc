AR=ar
ARFLAGS=rsv
FC=$(COMPILER_PREFIX)nvfortran$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -mp -KPIC -Mframe -Meh_frame -Minfo -Mdclchk -Mlarge_arrays -Mrecursive -Mstack_arrays -Kieee -Mfma -Mnodaz -Mnoflushz -Mnofpapprox -Mnofprelaxed -traceback
ifndef NDEBUG
FCFLAGS += -Mbounds -Mchkstk
endif # !NDEBUG
ifndef INTRIN
INTRIN=42
endif # !INTRIN
ifdef INTRIN
FCFLAGS += -DUSE_IEEE_INTRINSIC=$(INTRIN)
endif # INTRIN
