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
FCFLAGS=$(FCFLAGS) /DNDEBUG /Qopt-report:5
LDFLAGS=/link /RELEASE
!ELSE # !NDEBUG
FCFLAGS=$(FCFLAGS) /dbglibs /debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all
LDFLAGS=/link /DEBUG
!ENDIF # ?NDEBUG
!IFNDEF PLAT
PLAT=ifort\$(OS)-$(ARCH)-$(ABI)$(DEBUG)
!ENDIF # !PLAT
MKFS=Makefile
OBJS=..\obj\$(PLAT)\sksvd2.obj ..\obj\$(PLAT)\cksvd2.obj ..\obj\$(PLAT)\dksvd2.obj ..\obj\$(PLAT)\zksvd2.obj ..\obj\$(PLAT)\qksvd2.obj ..\obj\$(PLAT)\yksvd2.obj
EXES=..\bin\$(PLAT)\sksvd2.exe ..\bin\$(PLAT)\dksvd2.exe ..\bin\$(PLAT)\qksvd2.exe
LIBS=..\lib\$(PLAT)\ksvd2.lib

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

..\bin\$(PLAT)\sksvd2.exe: sksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ sksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\dksvd2.exe: dksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ dksvd2x.f90 $(LDFLAGS) $(LIBS)

..\bin\$(PLAT)\qksvd2.exe: qksvd2x.f90 tksvd2.f90 $(LIBS) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ qksvd2x.f90 $(LDFLAGS) $(LIBS)

clean:
	-del /f /q $(EXES)
	-del /f /q $(LIBS)
	-del /f /q $(OBJS)

purge:
	-rd ..\bin /s /q
	-rd ..\lib /s /q
	-rd ..\obj /s /q
