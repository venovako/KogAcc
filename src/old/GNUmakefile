### from dynord

../lib/$(PLAT)/libdynord.a: $(OBJS_DYNORD) $(MKFS) dynord/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_DYNORD)

../obj/$(PLAT)/sabsg.$(OXT): dynord/sabsg.F90 dynord/gabsg.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/sabsg.F90

../obj/$(PLAT)/cabsg.$(OXT): dynord/cabsg.F90 dynord/habsg.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/cabsg.F90

../obj/$(PLAT)/dabsg.$(OXT): dynord/dabsg.F90 dynord/gabsg.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/dabsg.F90

../obj/$(PLAT)/zabsg.$(OXT): dynord/zabsg.F90 dynord/habsg.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/zabsg.F90

../obj/$(PLAT)/smkwpq.$(OXT): dynord/smkwpq.F90 dynord/gmkwpq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/smkwpq.F90

../obj/$(PLAT)/cmkwpq.$(OXT): dynord/cmkwpq.F90 dynord/hmkwpq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/cmkwpq.F90

../obj/$(PLAT)/dmkwpq.$(OXT): dynord/dmkwpq.F90 dynord/gmkwpq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/dmkwpq.F90

../obj/$(PLAT)/zmkwpq.$(OXT): dynord/zmkwpq.F90 dynord/hmkwpq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/zmkwpq.F90

../obj/$(PLAT)/smk3pq.$(OXT): dynord/smk3pq.F90 dynord/gmk3pq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/smk3pq.F90

../obj/$(PLAT)/cmk3pq.$(OXT): dynord/cmk3pq.F90 dynord/gmk3pq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/cmk3pq.F90

../obj/$(PLAT)/dmk3pq.$(OXT): dynord/dmk3pq.F90 dynord/gmk3pq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/dmk3pq.F90

../obj/$(PLAT)/zmk3pq.$(OXT): dynord/zmk3pq.F90 dynord/gmk3pq.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/zmk3pq.F90

../obj/$(PLAT)/spqcmp.$(OXT): dynord/spqcmp.F90 dynord/gpqcmp.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/spqcmp.F90

../obj/$(PLAT)/dpqcmp.$(OXT): dynord/dpqcmp.F90 dynord/gpqcmp.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/dpqcmp.F90

../obj/$(PLAT)/spqmrg.$(OXT): dynord/spqmrg.F90 dynord/gpqmrg.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/spqmrg.F90

../obj/$(PLAT)/dpqmrg.$(OXT): dynord/dpqmrg.F90 dynord/gpqmrg.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/dpqmrg.F90

../obj/$(PLAT)/spqsrt.$(OXT): dynord/spqsrt.F90 dynord/gpqsrt.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/spqsrt.F90

../obj/$(PLAT)/dpqsrt.$(OXT): dynord/dpqsrt.F90 dynord/gpqsrt.F90 $(MKFS) dynord/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/dynord/dpqsrt.F90

### from test

../bin/$(PLAT)/spqsort.exe: test/spqsortx.F90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o spqsort.exe ../../../src/test/spqsortx.F90 -L../../../lib/$(PLAT) -ldynord $(LDFLAGS)

../bin/$(PLAT)/dpqsort.exe: test/dpqsortx.F90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	cd ../bin/$(PLAT) && $(FC) $(FLFLAGS) -o dpqsort.exe ../../../src/test/dpqsortx.F90 -L../../../lib/$(PLAT) -ldynord $(LDFLAGS)
