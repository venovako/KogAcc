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
OBJ=..\obj\$(PLAT)\sksvd2.obj ..\obj\$(PLAT)\dksvd2.obj ..\obj\$(PLAT)\qksvd2.obj
EXE=..\bin\$(PLAT)\sksvd2.exe ..\bin\$(PLAT)\dksvd2.exe ..\bin\$(PLAT)\qksvd2.exe
STA=..\lib\$(PLAT)\ksvd2.lib

all: dirs $(STA) $(EXE)
	@echo $(PLAT)

help:
	@echo "nmake.exe [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

dirs:
	-md ..\bin\$(PLAT)
	-md ..\lib\$(PLAT)
	-md ..\obj\$(PLAT)

doc: ..\Doxyfile
	cd .. && doxygen && cd src

$(STA): $(OBJ) $(MKFS)
	$(AR) $(ARFLAGS) /OUT:$@ $(OBJ)

..\obj\$(PLAT)\sksvd2.obj: sksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ sksvd2.f90

..\obj\$(PLAT)\dksvd2.obj: dksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ dksvd2.f90

..\obj\$(PLAT)\qksvd2.obj: qksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ qksvd2.f90

..\obj\$(PLAT)\xksvd2.obj: xksvd2.f90 gksvd2.f90 $(MKFS)
	$(FC) $(FCFLAGS) /c /Fo$@ xksvd2.f90

..\bin\$(PLAT)\sksvd2.exe: sksvd2x.f90 tksvd2.f90 $(STA) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ sksvd2x.f90 $(LDFLAGS) $(STA)

..\bin\$(PLAT)\dksvd2.exe: dksvd2x.f90 tksvd2.f90 $(STA) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ dksvd2x.f90 $(LDFLAGS) $(STA)

..\bin\$(PLAT)\qksvd2.exe: qksvd2x.f90 tksvd2.f90 $(STA) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ qksvd2x.f90 $(LDFLAGS) $(STA)

..\bin\$(PLAT)\xksvd2.exe: xksvd2x.f90 tksvd2.f90 $(STA) $(MKFS)
	$(FC) $(FCFLAGS) /Fe$@ xksvd2x.f90 $(LDFLAGS) $(STA)

clean:
	-del /f /q $(EXE)
	-del /f /q $(STA)
	-del /f /q $(OBJ)

purge:
	-rd ..\bin /s /q
	-rd ..\lib /s /q
	-rd ..\obj /s /q
