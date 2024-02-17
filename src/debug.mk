ifdef NDEBUG
DEBUG=
FCFLAGS += -DNDEBUG=$(NDEBUG)
else # !NDEBUG
DEBUG=g
endif # ?NDEBUG
ifdef ANIMATE
FCFLAGS += -DANIMATE=$(ANIMATE)
endif # ANIMATE
ifdef CR_MATH
FCFLAGS += -DCR_MATH=$(CR_MATH)
ifdef NDEBUG
# modified routines that do not reference errno
OBJS_CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf_noerrno.o
OBJS_CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot_noerrno.o
else # DEBUG
OBJS_CRF_MATH=$(CR_MATH)/src/binary32/hypot/hypotf.o
OBJS_CRD_MATH=$(CR_MATH)/src/binary64/hypot/hypot.o
endif # ?NDEBUG
else # !CR_MATH
ifdef NDEBUG
FCFLAGS += -DCR_MATH=bundled_noerrno
else # !NDEBUG
FCFLAGS += -DCR_MATH=bundled
endif # ?NDEBUG
endif # ?CR_MATH
