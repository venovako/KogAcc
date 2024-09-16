ifdef NDEBUG
DEBUG=
FCFLAGS += -DNDEBUG=$(NDEBUG) -DCR_MATH=bundled_noerrno
else # !NDEBUG
DEBUG=g
FCFLAGS += -DCR_MATH=bundled
endif # ?NDEBUG
ifdef ANIMATE
FCFLAGS += -DANIMATE=$(ANIMATE)
ifneq ($(COMPILER),gfortran)
GNU=$(realpath $(shell gcc -print-file-name=libquadmath.a))
endif # !gfortran
endif # ANIMATE
ifdef PROFILE
FCFLAGS += -DPVN_PROFILE=$(PROFILE)u -fno-inline -finstrument-functions
endif # PROFILE
