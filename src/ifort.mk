AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=$(COMPILER_PREFIX)ifort$(COMPILER_SUFFIX)
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -qopenmp -qopt-multi-version-aggressive -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -traceback -fp-model precise -fprotect-parens -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -standard-semantics -qsimd-honor-fp-model -qsimd-serialize-fp-reduction
ifdef NDEBUG
FCFLAGS += -qopt-report=5 -inline-level=2 -vec-threshold0
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -check all -fp-stack-check
endif # ?NDEBUG
# makes no sense to use CR_MATH without enforcing the FMAs in the rest of the code
ifdef CR_MATH
# ifort supports IEEE_FMA
FCFLAGS += -DUSE_IEEE_INTRINSIC
endif # CR_MATH
ifeq ($(OS),Linux)
FCFLAGS += -static-libgcc
endif # Linux
