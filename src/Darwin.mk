LAPACK=-L$(MKLROOT)/lib -Wl,-rpath,$(MKLROOT)/lib -lmkl_intel_$(ABI) -lmkl_sequential -lmkl_core -lpthread -lm -ldl
