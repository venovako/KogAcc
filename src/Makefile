OS=Windows
ARCH=x64
AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
FC=ifort.exe
!IFNDEF ABI
ABI=ilp64
!ENDIF # !ABI
FCFLAGS=/nologo
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8
!ENDIF # ilp64
!IFDEF NDEBUG
DEBUG=
FCFLAGS=$(FCFLAGS) /O$(NDEBUG)
!ELSE # !NDEBUG
DEBUG=d
FCFLAGS=$(FCFLAGS) /Od
!ENDIF # ?NDEBUG
FCFLAGS=$(FCFLAGS) /QxHost /Qopenmp /Qopt-multi-version-aggressive /fp:precise /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt /standard-semantics /traceback /libs:dll /threads
!IFDEF NDEBUG
FCFLAGS=$(FCFLAGS) /DNDEBUG /Qopt-report:5 /Ob2 /Qvec-threshold:0
LDFLAGS=/link /RELEASE
!ELSE # !NDEBUG
FCFLAGS=$(FCFLAGS) /dbglibs /debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all
LDFLAGS=/link /DEBUG
!ENDIF # ?NDEBUG
!IFDEF CR_MATH
FCFLAGS=$(FCFLAGS) /DUSE_IEEE_INTRINSIC
INTRIN=i
!ENDIF # CR_MATH
!IFNDEF PLAT
PLAT=ifort\$(OS)-$(ARCH)-$(ABI)$(DEBUG)
!ENDIF # !PLAT
MKFS=Makefile
OBJS_ARRBIO=..\obj\$(PLAT)\bfopen.obj ..\obj\$(PLAT)\ibrd1.obj ..\obj\$(PLAT)\sbrd1.obj ..\obj\$(PLAT)\cbrd1.obj ..\obj\$(PLAT)\dbrd1.obj ..\obj\$(PLAT)\zbrd1.obj ..\obj\$(PLAT)\qbrd1.obj ..\obj\$(PLAT)\ibwr1.obj ..\obj\$(PLAT)\sbwr1.obj ..\obj\$(PLAT)\cbwr1.obj ..\obj\$(PLAT)\dbwr1.obj ..\obj\$(PLAT)\zbwr1.obj ..\obj\$(PLAT)\qbwr1.obj ..\obj\$(PLAT)\ibrd2.obj ..\obj\$(PLAT)\sbrd2.obj ..\obj\$(PLAT)\cbrd2.obj ..\obj\$(PLAT)\dbrd2.obj ..\obj\$(PLAT)\zbrd2.obj ..\obj\$(PLAT)\qbrd2.obj ..\obj\$(PLAT)\ibwr2.obj ..\obj\$(PLAT)\sbwr2.obj ..\obj\$(PLAT)\cbwr2.obj ..\obj\$(PLAT)\dbwr2.obj ..\obj\$(PLAT)\zbwr2.obj ..\obj\$(PLAT)\qbwr2.obj
OBJS_BLKSVD=..\obj\$(PLAT)\sbrdg.obj ..\obj\$(PLAT)\cbrdg.obj ..\obj\$(PLAT)\dbrdg.obj ..\obj\$(PLAT)\zbrdg.obj ..\obj\$(PLAT)\sbordg.obj ..\obj\$(PLAT)\cbordg.obj ..\obj\$(PLAT)\dbordg.obj ..\obj\$(PLAT)\zbordg.obj ..\obj\$(PLAT)\nb2m.obj
OBJS_DYNORD=..\obj\$(PLAT)\dsoffsq.obj ..\obj\$(PLAT)\dcoffsq.obj ..\obj\$(PLAT)\qdoffsq.obj ..\obj\$(PLAT)\qzoffsq.obj ..\obj\$(PLAT)\spqcmp.obj ..\obj\$(PLAT)\spqmrg.obj ..\obj\$(PLAT)\spqsrt.obj ..\obj\$(PLAT)\spqsort.obj ..\obj\$(PLAT)\dpqcmp.obj ..\obj\$(PLAT)\dpqmrg.obj ..\obj\$(PLAT)\dpqsrt.obj ..\obj\$(PLAT)\dpqsort.obj ..\obj\$(PLAT)\qpqcmp.obj ..\obj\$(PLAT)\qpqmrg.obj ..\obj\$(PLAT)\qpqsrt.obj ..\obj\$(PLAT)\qpqsort.obj ..\obj\$(PLAT)\sosum4.obj ..\obj\$(PLAT)\dosum4.obj ..\obj\$(PLAT)\spsum4.obj ..\obj\$(PLAT)\dpsum4.obj ..\obj\$(PLAT)\qpsum4.obj
OBJS_KSVD2=..\obj\$(PLAT)\sksvd2.obj ..\obj\$(PLAT)\cksvd2.obj ..\obj\$(PLAT)\dksvd2.obj ..\obj\$(PLAT)\zksvd2.obj ..\obj\$(PLAT)\qksvd2.obj ..\obj\$(PLAT)\yksvd2.obj ..\obj\$(PLAT)\slwsv2.obj ..\obj\$(PLAT)\dlwsv2.obj
OBJS_FAUX=..\obj\$(PLAT)\slango.obj ..\obj\$(PLAT)\clango.obj ..\obj\$(PLAT)\dlango.obj ..\obj\$(PLAT)\zlango.obj ..\obj\$(PLAT)\sthalt.obj
EXES_DYNORD=..\bin\$(PLAT)\spqsort.exe ..\bin\$(PLAT)\dpqsort.exe ..\bin\$(PLAT)\qpqsort.exe
EXES_KSVD2=..\bin\$(PLAT)\sksvd2.exe ..\bin\$(PLAT)\cksvd2.exe ..\bin\$(PLAT)\dksvd2.exe ..\bin\$(PLAT)\zksvd2.exe ..\bin\$(PLAT)\qksvd2.exe ..\bin\$(PLAT)\yksvd2.exe ..\bin\$(PLAT)\slwsv2.exe ..\bin\$(PLAT)\dlwsv2.exe ..\bin\$(PLAT)\srnd.exe
OBJS=$(OBJS_ARRBIO) $(OBJS_BLKSVD) $(OBJS_DYNORD) $(OBJS_KSVD2) $(OBJS_FAUX)
LIBS=..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\blksvd.lib ..\lib\$(PLAT)\dynord.lib ..\lib\$(PLAT)\ksvd2.lib ..\lib\$(PLAT)\faux.lib
EXES=$(EXES_KSVD2) $(EXES_DYNORD)
LAPACK=mkl_intel_$(ABI)_dll.lib mkl_sequential_dll.lib mkl_core_dll.lib

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo "nmake.exe [ABI=ilp64|lp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

dirs:
	-md ..\bin\$(PLAT)
	-md ..\lib\$(PLAT)
	-md ..\obj\$(PLAT)

doc: ..\Doxyfile
	cd .. && doxygen && cd src

..\lib\$(PLAT)\arrbio.lib: $(OBJS_ARRBIO) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_ARRBIO)

..\lib\$(PLAT)\blksvd.lib: $(OBJS_BLKSVD) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_BLKSVD)

..\lib\$(PLAT)\dynord.lib: $(OBJS_DYNORD) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_DYNORD)

..\lib\$(PLAT)\ksvd2.lib: $(OBJS_KSVD2) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_KSVD2)

..\lib\$(PLAT)\faux.lib: $(OBJS_FAUX) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_FAUX)

..\obj\$(PLAT)\sksvd2.obj: sksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c sksvd2.F90 /Fo$@

..\obj\$(PLAT)\cksvd2.obj: cksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c cksvd2.F90 /Fo$@

..\obj\$(PLAT)\dksvd2.obj: dksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c dksvd2.F90 /Fo$@

..\obj\$(PLAT)\zksvd2.obj: zksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c zksvd2.F90 /Fo$@

..\obj\$(PLAT)\qksvd2.obj: qksvd2.F90 gksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c qksvd2.F90 /Fo$@

..\obj\$(PLAT)\yksvd2.obj: yksvd2.F90 hksvd2$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c yksvd2.F90 /Fo$@

..\obj\$(PLAT)\slwsv2.obj: slwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c slwsv2.f90 /Fo$@

..\obj\$(PLAT)\dlwsv2.obj: dlwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dlwsv2.f90 /Fo$@

..\obj\$(PLAT)\sbrdg.obj: sbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c sbrdg.f90 /Fo$@

..\obj\$(PLAT)\cbrdg.obj: cbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c cbrdg.f90 /Fo$@

..\obj\$(PLAT)\dbrdg.obj: dbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dbrdg.f90 /Fo$@

..\obj\$(PLAT)\zbrdg.obj: zbrdg.f90 gbrdg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zbrdg.f90 /Fo$@

..\obj\$(PLAT)\sbordg.obj: sbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c sbordg.f90 /Fo$@

..\obj\$(PLAT)\cbordg.obj: cbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c cbordg.f90 /Fo$@

..\obj\$(PLAT)\dbordg.obj: dbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dbordg.f90 /Fo$@

..\obj\$(PLAT)\zbordg.obj: zbordg.f90 gbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zbordg.f90 /Fo$@

..\obj\$(PLAT)\nb2m.obj: nb2m.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c nb2m.f90 /Fo$@

..\obj\$(PLAT)\dsoffsq.obj: dsoffsq.F90 goffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c dsoffsq.F90 /Fo$@

..\obj\$(PLAT)\dcoffsq.obj: dcoffsq.F90 hoffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c dcoffsq.F90 /Fo$@

..\obj\$(PLAT)\qdoffsq.obj: qdoffsq.F90 goffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c qdoffsq.F90 /Fo$@

..\obj\$(PLAT)\qzoffsq.obj: qzoffsq.F90 hoffsq$(INTRIN).F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c qzoffsq.F90 /Fo$@

..\obj\$(PLAT)\spqcmp.obj: spqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c spqcmp.f90 /Fo$@

..\obj\$(PLAT)\dpqcmp.obj: dpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dpqcmp.f90 /Fo$@

..\obj\$(PLAT)\qpqcmp.obj: qpqcmp.f90 gpqcmp.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qpqcmp.f90 /Fo$@

..\obj\$(PLAT)\spqmrg.obj: spqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c spqmrg.f90 /Fo$@

..\obj\$(PLAT)\dpqmrg.obj: dpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dpqmrg.f90 /Fo$@

..\obj\$(PLAT)\qpqmrg.obj: qpqmrg.f90 gpqmrg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qpqmrg.f90 /Fo$@

..\obj\$(PLAT)\spqsrt.obj: spqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c spqsrt.F90 /Fo$@

..\obj\$(PLAT)\dpqsrt.obj: dpqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c dpqsrt.F90 /Fo$@

..\obj\$(PLAT)\qpqsrt.obj: qpqsrt.F90 gpqsrt.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c qpqsrt.F90 /Fo$@

..\obj\$(PLAT)\spqsort.obj: spqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c spqsort.F90 /Fo$@

..\obj\$(PLAT)\dpqsort.obj: dpqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c dpqsort.F90 /Fo$@

..\obj\$(PLAT)\qpqsort.obj: qpqsort.F90 gpqsort.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c qpqsort.F90 /Fo$@

..\obj\$(PLAT)\sosum4.obj: sosum4.F90 gosum4.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c sosum4.F90 /Fo$@

..\obj\$(PLAT)\dosum4.obj: dosum4.F90 gosum4.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c dosum4.F90 /Fo$@

..\obj\$(PLAT)\spsum4.obj: spsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c spsum4.f90 /Fo$@

..\obj\$(PLAT)\dpsum4.obj: dpsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dpsum4.f90 /Fo$@

..\obj\$(PLAT)\qpsum4.obj: qpsum4.f90 gpsum4.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qpsum4.f90 /Fo$@

..\obj\$(PLAT)\bfopen.obj: bfopen.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c bfopen.f90 /Fo$@

..\obj\$(PLAT)\ibrd1.obj: ibrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c ibrd1.f90 /Fo$@

..\obj\$(PLAT)\sbrd1.obj: sbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c sbrd1.f90 /Fo$@

..\obj\$(PLAT)\cbrd1.obj: cbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c cbrd1.f90 /Fo$@

..\obj\$(PLAT)\dbrd1.obj: dbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dbrd1.f90 /Fo$@

..\obj\$(PLAT)\zbrd1.obj: zbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zbrd1.f90 /Fo$@

..\obj\$(PLAT)\qbrd1.obj: qbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qbrd1.f90 /Fo$@

..\obj\$(PLAT)\ibwr1.obj: ibwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c ibwr1.f90 /Fo$@

..\obj\$(PLAT)\sbwr1.obj: sbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c sbwr1.f90 /Fo$@

..\obj\$(PLAT)\cbwr1.obj: cbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c cbwr1.f90 /Fo$@

..\obj\$(PLAT)\dbwr1.obj: dbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dbwr1.f90 /Fo$@

..\obj\$(PLAT)\zbwr1.obj: zbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zbwr1.f90 /Fo$@

..\obj\$(PLAT)\qbwr1.obj: qbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qbwr1.f90 /Fo$@

..\obj\$(PLAT)\ibrd2.obj: ibrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c ibrd2.f90 /Fo$@

..\obj\$(PLAT)\sbrd2.obj: sbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c sbrd2.f90 /Fo$@

..\obj\$(PLAT)\cbrd2.obj: cbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c cbrd2.f90 /Fo$@

..\obj\$(PLAT)\dbrd2.obj: dbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dbrd2.f90 /Fo$@

..\obj\$(PLAT)\zbrd2.obj: zbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zbrd2.f90 /Fo$@

..\obj\$(PLAT)\qbrd2.obj: qbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qbrd2.f90 /Fo$@

..\obj\$(PLAT)\ibwr2.obj: ibwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c ibwr2.f90 /Fo$@

..\obj\$(PLAT)\sbwr2.obj: sbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c sbwr2.f90 /Fo$@

..\obj\$(PLAT)\cbwr2.obj: cbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c cbwr2.f90 /Fo$@

..\obj\$(PLAT)\dbwr2.obj: dbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dbwr2.f90 /Fo$@

..\obj\$(PLAT)\zbwr2.obj: zbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zbwr2.f90 /Fo$@

..\obj\$(PLAT)\qbwr2.obj: qbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c qbwr2.f90 /Fo$@

..\obj\$(PLAT)\slango.obj: slango.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c slango.f90 /Fo$@

..\obj\$(PLAT)\clango.obj: clango.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c clango.f90 /Fo$@

..\obj\$(PLAT)\dlango.obj: dlango.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c dlango.f90 /Fo$@

..\obj\$(PLAT)\zlango.obj: zlango.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c zlango.f90 /Fo$@

..\obj\$(PLAT)\sthalt.obj: sthalt.F90 $(MKFS)
	$(FC) $(FCFLAGS) /fpp /c sthalt.F90 /Fo$@

..\bin\$(PLAT)\sksvd2.exe: sksvd2x.F90 tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp sksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\cksvd2.exe: cksvd2x.F90 uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp cksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\dksvd2.exe: dksvd2x.F90 tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp dksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\zksvd2.exe: zksvd2x.F90 uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp zksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\qksvd2.exe: qksvd2x.F90 tksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp qksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\yksvd2.exe: yksvd2x.F90 uksvd2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp yksvd2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\slwsv2.exe: slwsv2x.F90 tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp slwsv2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\dlwsv2.exe: dlwsv2x.F90 tlasv2.F90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /fpp dlwsv2x.F90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\srnd.exe: srnd.f90 $(MKFS)
	$(FC) $(FCFLAGS) srnd.f90 /Fe$@ $(LDFLAGS)

..\bin\$(PLAT)\spqsort.exe: spqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS)
	$(FC) $(FCFLAGS) spqsortx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\dpqsort.exe: dpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS)
	$(FC) $(FCFLAGS) dpqsortx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\dynord.lib

..\bin\$(PLAT)\qpqsort.exe: qpqsortx.f90 ..\lib\$(PLAT)\dynord.lib $(MKFS)
	$(FC) $(FCFLAGS) qpqsortx.f90 /Fe$@ $(LDFLAGS) ..\lib\$(PLAT)\dynord.lib

clean:
	-del /f /q $(EXES)
	-del /f /q $(LIBS)
	-del /f /q $(OBJS)

purge:
	-rd ..\bin /s /q
	-rd ..\lib /s /q
	-rd ..\obj /s /q
