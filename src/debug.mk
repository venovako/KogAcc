ifdef NDEBUG
DEBUG=
FCFLAGS += -DNDEBUG=$(NDEBUG)
else # !NDEBUG
DEBUG=g
endif # ?NDEBUG
ifdef ANIMATE
FCFLAGS += -DANIMATE=$(ANIMATE)
endif # ANIMATE
