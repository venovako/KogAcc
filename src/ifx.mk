ifdef LAPACK
FCFLAGS += -DLAPACK=$(LAPACK)
ifeq ($(ABI),ilp64)
FCFLAGS += -DMKL_ILP64=$(LAPACK)
LDFLAGS += -qmkl-$(ABI)=$(LAPACK)
else # lp64
FCFLAGS += -DMKL_LP64=$(LAPACK)
LDFLAGS += -qmkl=$(LAPACK)
endif # ?ABI
ifeq ($(LAPACK),sequential)
FCFLAGS += -DMKL_DIRECT_CALL_SEQ
else # parallel
FCFLAGS += -DMKL_DIRECT_CALL
endif # ?LAPACK
FCFLAGS += -I${MKLROOT}/include
LDFLAGS += $(shell if [ -L /usr/lib64/libmemkind.so ]; then echo '-lmemkind'; fi)
endif # LAPACK
ifndef LIBPVN
LIBPVN=$(realpath ../../../../libpvn)
endif # !LIBPVN
LDFLAGS += -L$(LIBPVN)/src -Wl,-rpath=$(LIBPVN)/src -lpvn -lquadmath -lgcc_s -lpthread -lm -ldl
