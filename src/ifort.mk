AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifort
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
#-DUSE_IEEE_INTRINSIC
FCFLAGS += -xHost -qopt-multi-version-aggressive -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -traceback -fp-model precise -fprotect-parens -fma -no-ftz -no-complex-limited-range -no-fast-transcendentals -prec-div -prec-sqrt -standard-semantics
ifdef NDEBUG
FCFLAGS += -qopt-report=5 -inline-level=2 -vec-threshold0
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames -fp-stack-check
endif # ?NDEBUG
ifeq ($(OS),Linux)
FCFLAGS += -static-libgcc
endif # Linux
