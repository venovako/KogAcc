AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifx$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -fPIC -fexceptions -fno-omit-frame-pointer -fp-model=precise -fp-speculation=safe -fimf-precision=high -fma -fprotect-parens -no-ftz -qopenmp -standard-semantics -traceback -vec-threshold0 -rdynamic -static-libgcc
ifdef NDEBUG
FCFLAGS += -inline-level=2 -qopt-report=3
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all
endif # ?NDEBUG
ifndef INTRIN
INTRIN=69
endif # !INTRIN
ifdef INTRIN
FCFLAGS += -DUSE_IEEE_INTRINSIC=$(INTRIN)
endif # INTRIN
ifeq ($(OS),Linux)
ifndef NDEBUG
FCFLAGS += -debug parallel
endif # !NDEBUG
endif # Linux
