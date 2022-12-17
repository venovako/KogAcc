AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -qopenmp -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -traceback -fp-model precise -fprotect-parens -fma -no-ftz -standard-semantics -static-libgcc
ifdef NDEBUG
FCFLAGS += -inline-level=2 -vec-threshold0 #-qopt-report=3
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all
endif # ?NDEBUG
# makes no sense to use CR_MATH without enforcing the FMAs in the rest of the code
ifdef CR_MATH
# ifx supports IEEE_FMA
FCFLAGS += -DUSE_IEEE_INTRINSIC
endif # CR_MATH