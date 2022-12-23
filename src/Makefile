OS=Windows
ARCH=x64
AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
!IFNDEF FORT
FORT=ifort
!ENDIF # !FORT
FC=$(FORT).exe
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
FCFLAGS=$(FCFLAGS) /QxHost /Qopenmp /fp:precise /Qfma /Qftz- /standard-semantics /traceback /libs:dll /threads
!IF "$(FORT)"=="ifort"
FCFLAGS=$(FCFLAGS) /Qopt-multi-version-aggressive /Qcomplex-limited-range- /Qfast-transcendentals- /Qprec-div /Qprec-sqrt
!ENDIF # ifort
!IFDEF NDEBUG
FCFLAGS=$(FCFLAGS) /DNDEBUG /Ob2 /Qvec-threshold:0
!IF "$(FORT)"=="ifort"
FCFLAGS=$(FCFLAGS) /Qopt-report:5
!ELSE # ifx
FCFLAGS=$(FCFLAGS) /Qopt-report:3
!ENDIF # ?FORT
LDFLAGS=/link /RELEASE
!ELSE # !NDEBUG
FCFLAGS=$(FCFLAGS) /dbglibs /debug:full /debug:inline-debug-info /debug-parameters:all /check:all /warn:all
LDFLAGS=/link /DEBUG
!ENDIF # ?NDEBUG
!IFDEF CR_MATH
FCFLAGS=$(FCFLAGS) /DUSE_IEEE_INTRINSIC $(CR_MATH)
INTRIN=i
!ENDIF # CR_MATH
!IFNDEF PLAT
PLAT=$(FORT)\$(OS)-$(ARCH)-$(ABI)$(DEBUG)
!ENDIF # !PLAT
MKFS=Makefile #obj.mk
!INCLUDE obj.mk
LAPACK=mkl_intel_$(ABI)_dll.lib mkl_sequential_dll.lib mkl_core_dll.lib

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo "nmake.exe [FORT=ifort|ifx] [ABI=ilp64|lp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help|purge]"

dirs:
	-md ..\bin\$(PLAT)
	-md ..\lib\$(PLAT)
	-md ..\obj\$(PLAT)

doc: ..\Doxyfile
	cd .. && doxygen && cd src

!INCLUDE arrbio\Makefile
!INCLUDE blksvd\Makefile
!INCLUDE dynord\Makefile
!INCLUDE ksvd2\Makefile
!INCLUDE faux\Makefile
!INCLUDE test\Makefile

clean:
	-del /f /q $(EXES)
	-del /f /q $(LIBS)
	-del /f /q $(OBJS)
	-del /f /q *.opt*
	-del /f /q *__genmod.*

purge:
	-rd ..\bin /s /q
	-rd ..\lib /s /q
	-rd ..\obj /s /q
