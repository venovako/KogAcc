OS=Windows
ARCH=x64
AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
FC=ifort.exe
!IFNDEF ABI
ABI=lp64
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
!IFNDEF PLAT
PLAT=ifort\$(OS)-$(ARCH)-$(ABI)$(DEBUG)
!ENDIF # !PLAT
MKFS=Makefile
OBJS_ARRBIO=..\obj\$(PLAT)\bfopen.obj ..\obj\$(PLAT)\sbrd1.obj ..\obj\$(PLAT)\cbrd1.obj ..\obj\$(PLAT)\dbrd1.obj ..\obj\$(PLAT)\zbrd1.obj ..\obj\$(PLAT)\sbwr1.obj ..\obj\$(PLAT)\cbwr1.obj ..\obj\$(PLAT)\dbwr1.obj ..\obj\$(PLAT)\zbwr1.obj ..\obj\$(PLAT)\sbrd2.obj ..\obj\$(PLAT)\cbrd2.obj ..\obj\$(PLAT)\dbrd2.obj ..\obj\$(PLAT)\zbrd2.obj ..\obj\$(PLAT)\sbwr2.obj ..\obj\$(PLAT)\cbwr2.obj ..\obj\$(PLAT)\dbwr2.obj ..\obj\$(PLAT)\zbwr2.obj
OBJS_BLKSVD=..\obj\$(PLAT)\sbordg.obj ..\obj\$(PLAT)\cbordg.obj ..\obj\$(PLAT)\dbordg.obj ..\obj\$(PLAT)\zbordg.obj ..\obj\$(PLAT)\nb2m.obj  ..\obj\$(PLAT)\dsoffsq.obj ..\obj\$(PLAT)\dcoffsq.obj ..\obj\$(PLAT)\qdoffsq.obj ..\obj\$(PLAT)\qzoffsq.obj
OBJS_KSVD2=..\obj\$(PLAT)\sksvd2.obj ..\obj\$(PLAT)\cksvd2.obj ..\obj\$(PLAT)\dksvd2.obj ..\obj\$(PLAT)\zksvd2.obj ..\obj\$(PLAT)\qksvd2.obj ..\obj\$(PLAT)\yksvd2.obj ..\obj\$(PLAT)\slwsv2.obj ..\obj\$(PLAT)\dlwsv2.obj
EXES_KSVD2=..\bin\$(PLAT)\sksvd2.exe ..\bin\$(PLAT)\cksvd2.exe ..\bin\$(PLAT)\dksvd2.exe ..\bin\$(PLAT)\zksvd2.exe ..\bin\$(PLAT)\qksvd2.exe ..\bin\$(PLAT)\yksvd2.exe ..\bin\$(PLAT)\slwsv2.exe ..\bin\$(PLAT)\dlwsv2.exe
OBJS=$(OBJS_ARRBIO) $(OBJS_BLKSVD) $(OBJS_KSVD2)
LIBS=..\lib\$(PLAT)\arrbio.lib ..\lib\$(PLAT)\blksvd.a ..\lib\$(PLAT)\ksvd2.lib
EXES=$(EXES_KSVD2) ..\bin\$(PLAT)\srnd.exe
LAPACK=mkl_intel_$(ABI)_dll.lib mkl_sequential_dll.lib mkl_core_dll.lib

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo "nmake.exe [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

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

..\lib\$(PLAT)\ksvd2.lib: $(OBJS_KSVD2) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS_KSVD2)

..\obj\$(PLAT)\sksvd2.obj: sksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sksvd2.f90

..\obj\$(PLAT)\cksvd2.obj: cksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ cksvd2.f90

..\obj\$(PLAT)\dksvd2.obj: dksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dksvd2.f90

..\obj\$(PLAT)\zksvd2.obj: zksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ zksvd2.f90

..\obj\$(PLAT)\qksvd2.obj: qksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ qksvd2.f90

..\obj\$(PLAT)\yksvd2.obj: yksvd2.f90 hksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ yksvd2.f90

..\obj\$(PLAT)\slwsv2.obj: slwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ slwsv2.f90

..\obj\$(PLAT)\dlwsv2.obj: dlwsv2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dlwsv2.f90

..\obj\$(PLAT)\sbordg.obj: sbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sbordg.f90

..\obj\$(PLAT)\cbordg.obj: cbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ cbordg.f90

..\obj\$(PLAT)\dbordg.obj: dbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dbordg.f90

..\obj\$(PLAT)\zbordg.obj: zbordg.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ zbordg.f90

..\obj\$(PLAT)\nb2m.obj: nb2m.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ nb2m.f90

..\obj\$(PLAT)\dsoffsq.obj: dsoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dsoffsq.f90

..\obj\$(PLAT)\dcoffsq.obj: dcoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dcoffsq.f90

..\obj\$(PLAT)\qdoffsq.obj: qdoffsq.f90 goffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ qdoffsq.f90

..\obj\$(PLAT)\qzoffsq.obj: qzoffsq.f90 hoffsq.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ qzoffsq.f90

..\obj\$(PLAT)\bfopen.obj: bfopen.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ bfopen.f90

..\obj\$(PLAT)\sbrd1.obj: sbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sbrd1.f90

..\obj\$(PLAT)\cbrd1.obj: cbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ cbrd1.f90

..\obj\$(PLAT)\dbrd1.obj: dbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dbrd1.f90

..\obj\$(PLAT)\zbrd1.obj: zbrd1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ zbrd1.f90

..\obj\$(PLAT)\sbwr1.obj: sbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sbwr1.f90

..\obj\$(PLAT)\cbwr1.obj: cbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ cbwr1.f90

..\obj\$(PLAT)\dbwr1.obj: dbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dbwr1.f90

..\obj\$(PLAT)\zbwr1.obj: zbwr1.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ zbwr1.f90

..\obj\$(PLAT)\sbrd2.obj: sbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sbrd2.f90

..\obj\$(PLAT)\cbrd2.obj: cbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ cbrd2.f90

..\obj\$(PLAT)\dbrd2.obj: dbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dbrd2.f90

..\obj\$(PLAT)\zbrd2.obj: zbrd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ zbrd2.f90

..\obj\$(PLAT)\sbwr2.obj: sbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sbwr2.f90

..\obj\$(PLAT)\cbwr2.obj: cbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ cbwr2.f90

..\obj\$(PLAT)\dbwr2.obj: dbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dbwr2.f90

..\obj\$(PLAT)\zbwr2.obj: zbwr2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ zbwr2.f90

..\bin\$(PLAT)\sksvd2.exe: sksvd2x.f90 tksvd2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ sksvd2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\cksvd2.exe: cksvd2x.f90 uksvd2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ cksvd2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\dksvd2.exe: dksvd2x.f90 tksvd2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ dksvd2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\zksvd2.exe: zksvd2x.f90 uksvd2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ zksvd2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\qksvd2.exe: qksvd2x.f90 tksvd2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ qksvd2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\yksvd2.exe: yksvd2x.f90 uksvd2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ yksvd2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib

..\bin\$(PLAT)\slwsv2.exe: slwsv2x.f90 tlasv2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ slwsv2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\dlwsv2.exe: dlwsv2x.f90 tlasv2.f90 ..\lib\$(PLAT)\ksvd2.lib $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ dlwsv2x.f90 $(LDFLAGS) ..\lib\$(PLAT)\ksvd2.lib $(LAPACK)

..\bin\$(PLAT)\srnd.exe: srnd.f90 $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ srnd.f90

clean:
	-del /f /q $(EXES)
	-del /f /q $(LIBS)
	-del /f /q $(OBJS)

purge:
	-rd ..\bin /s /q
	-rd ..\lib /s /q
	-rd ..\obj /s /q
