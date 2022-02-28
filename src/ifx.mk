AR=xiar
ARFLAGS=-qnoipo -lib rsv
FC=ifx
FCFLAGS=-DUSE_IEEE_INTRINSIC
ifdef NDEBUG
FCFLAGS += -O$(NDEBUG)
else # !NDEBUG
FCFLAGS += -O0 -g
endif # ?NDEBUG
FCFLAGS += -xHost -fPIC -fexceptions -fno-omit-frame-pointer -rdynamic -traceback -fp-model precise -fprotect-parens -fma -no-ftz -standard-semantics -static-libgcc
ifdef NDEBUG
FCFLAGS += -qopt-report=3
else # !NDEBUG
FCFLAGS += -debug emit_column -debug extended -debug inline-debug-info -debug pubnames
endif # ?NDEBUG
