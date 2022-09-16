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
FCFLAGS=$(FCFLAGS) /QxHost /Qopt-multi-version-aggressive /fp:precise /Qfma /Qftz- /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt /standard-semantics /traceback /libs:dll /threads
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
OBJS=..\obj\$(PLAT)\sksvd2.obj ..\obj\$(PLAT)\cksvd2.obj ..\obj\$(PLAT)\dksvd2.obj ..\obj\$(PLAT)\zksvd2.obj ..\obj\$(PLAT)\qksvd2.obj ..\obj\$(PLAT)\yksvd2.obj ..\obj\$(PLAT)\slwsv2.obj ..\obj\$(PLAT)\dlwsv2.obj ..\obj\$(PLAT)\sbordg.obj ..\obj\$(PLAT)\cbordg.obj ..\obj\$(PLAT)\dbordg.obj ..\obj\$(PLAT)\zbordg.obj ..\obj\$(PLAT)\bfopen.obj ..\obj\$(PLAT)\nb2m.obj
EXES=..\bin\$(PLAT)\sksvd2.exe ..\bin\$(PLAT)\cksvd2.exe ..\bin\$(PLAT)\dksvd2.exe ..\bin\$(PLAT)\zksvd2.exe ..\bin\$(PLAT)\qksvd2.exe ..\bin\$(PLAT)\yksvd2.exe ..\bin\$(PLAT)\slwsv2.exe ..\bin\$(PLAT)\dlwsv2.exe ..\bin\$(PLAT)\srnd.exe
LIBS=..\lib\$(PLAT)\ksvd2.lib
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

$(LIBS): $(OBJS) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJS)

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

..\obj\$(PLAT)\bfopen.obj: bfopen.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ bfopen.f90

..\obj\$(PLAT)\nb2m.obj: nb2m.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ nb2m.f90

..\bin\$(PLAT)\sksvd2.exe: sksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ sksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\cksvd2.exe: cksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ cksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\dksvd2.exe: dksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ dksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\zksvd2.exe: zksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ zksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\qksvd2.exe: qksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ qksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\yksvd2.exe: yksvd2x.f90 uksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ yksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\slwsv2.exe: slwsv2x.f90 tlasv2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ slwsv2x.f90 $(LDFLAGS) $(LIBS) $(LAPACK)

..\bin\$(PLAT)\dlwsv2.exe: dlwsv2x.f90 tlasv2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ dlwsv2x.f90 $(LDFLAGS) $(LIBS) $(LAPACK)

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
