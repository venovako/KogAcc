AR=ar
ARFLAGS=rsv
FC=nvfortran
FCFLAGS= #-DUSE_IEEE_INTRINSIC
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g
endif # ?NDEBUG
FCFLAGS += -KPIC -Mframe -Meh_frame -Minfo -Mdclchk -Mlarge_arrays -Mrecursive -Mstack_arrays -Kieee -Mfma -Mnodaz -Mnoflushz -Mnofpapprox -Mnofprelaxed -traceback
ifndef NDEBUG
FCFLAGS += -Mbounds -Mchkstk
endif # !NDEBUG
