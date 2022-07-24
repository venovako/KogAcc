AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifx
ifdef NDEBUG
FCFLAGS=-O$(NDEBUG)
else # !NDEBUG
FCFLAGS=-O0 -g
endif # ?NDEBUG
#-DUSE_IEEE_INTRINSIC
FCFLAGS += -xHost -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -traceback -fp-model precise -fprotect-parens -fma -no-ftz -standard-semantics -static-libgcc
ifdef NDEBUG
FCFLAGS += -qopt-report=3 -inline-level=2 -vec-threshold0
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames
endif # ?NDEBUG
