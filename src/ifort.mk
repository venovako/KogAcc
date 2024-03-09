# ifort has been deprecated
AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifort$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -diag-disable=10397,10448 -xHost -fPIC -fexceptions -fno-omit-frame-pointer -qopt-multi-version-aggressive -qopt-zmm-usage=high -fp-model precise -fma -fprotect-parens -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -qopenmp -qsimd-honor-fp-model -qsimd-serialize-fp-reduction -standard-semantics -traceback -vec-threshold0 -rdynamic # use -xCOMMON-AVX512 for Intel Xeon Phis
ifdef NDEBUG
FCFLAGS += -qopt-report=5 -inline-level=2
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all -fp-stack-check
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
FCFLAGS += -static-libgcc
endif # Linux
