ifdef NDEBUG
DEBUG=
FCFLAGS += -DNDEBUG=$(NDEBUG) -DCR_MATH=bundled_noerrno
else # !NDEBUG
DEBUG=g
FCFLAGS += -DCR_MATH=bundled
endif # ?NDEBUG
ifdef ANIMATE
FCFLAGS += -DANIMATE=$(ANIMATE)
ifeq ($(findstring 86,$(ARCH)),86)
ifneq ($(COMPILER),gfortran)
GNU=$(realpath $(shell gcc -print-file-name=libquadmath.a))
endif # !gfortran
endif # 86
endif # ANIMATE
ifdef PROFILE
FCFLAGS += -DPVN_PROFILE=$(PROFILE)u -fno-inline -finstrument-functions
endif # PROFILE
