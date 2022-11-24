../bin/$(PLAT)/sksvd2.exe: test/sksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/sksvd2x.F90 $(OBJS_CRF_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/cksvd2.exe: test/cksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/cksvd2x.F90 $(OBJS_CRF_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/dksvd2.exe: test/dksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/dksvd2x.F90 $(OBJS_CRD_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/zksvd2.exe: test/zksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/zksvd2x.F90 $(OBJS_CRD_MATH) -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/qksvd2.exe: test/qksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/qksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/yksvd2.exe: test/yksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/yksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/xksvd2.exe: test/xksvd2x.F90 test/tksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/xksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/wksvd2.exe: test/wksvd2x.F90 test/uksvd2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/wksvd2x.F90 -L../lib/$(PLAT) -lksvd2

../bin/$(PLAT)/slwsv2.exe: test/slwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/slwsv2x.F90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/dlwsv2.exe: test/dlwsv2x.F90 test/tlasv2.F90 ../lib/$(PLAT)/libksvd2.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/dlwsv2x.F90 -L../lib/$(PLAT) -lksvd2 $(LAPACK)

../bin/$(PLAT)/srnd.exe: test/srnd.f90 $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/srnd.f90

../bin/$(PLAT)/spqsort.exe: test/spqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/spqsortx.f90 -L../lib/$(PLAT) -ldynord

../bin/$(PLAT)/dpqsort.exe: test/dpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/dpqsortx.f90 -L../lib/$(PLAT) -ldynord

../bin/$(PLAT)/qpqsort.exe: test/qpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/qpqsortx.f90 -L../lib/$(PLAT) -ldynord

../bin/$(PLAT)/xpqsort.exe: test/xpqsortx.f90 ../lib/$(PLAT)/libdynord.a $(MKFS) test/GNUmakefile
	$(FC) $(FCFLAGS) -o $@ test/xpqsortx.f90 -L../lib/$(PLAT) -ldynord

