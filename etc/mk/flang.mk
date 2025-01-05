# YMMV
$(warning Flang is NOT supported, use it at your own risk!)
AR=ar
ARFLAGS=rsv
FC=flang-new
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # DEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
ifndef MARCH
MARCH=native
endif # !MARCH
FCFLAGS=-cpp -march=$(MARCH) -fPIC -fno-omit-frame-pointer -ffp-contract=fast -fhonor-infinities -fhonor-nans -fimplicit-none #-fopenmp
ifeq ($(ARCH),Darwin)
FCFLAGS += -fintegrated-as
endif # Darwin
FCFLAGS += -rdynamic
