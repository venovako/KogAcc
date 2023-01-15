OS=Windows
ARCH=x64
AR=xilib.exe
ARFLAGS=-qnoipo -lib /NOLOGO /VERBOSE
!IFNDEF FORT
FORT=ifort
!ENDIF # !FORT
FC=$(FORT).exe
!IFNDEF ABI
ABI=lp64
!ENDIF # !ABI
FCFLAGS=/nologo
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8 /DUSE_ILP64=$(ABI)
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
FCFLAGS=$(FCFLAGS) /DNDEBUG=$(NDEBUG) /Ob2 /Qvec-threshold:0
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
!IFNDEF INTRIN
INTRIN=IEEE_FMA
!ENDIF # !INTRIN
!IFDEF INTRIN
FCFLAGS=$(FCFLAGS) /DUSE_IEEE_INTRINSIC=$(INTRIN)
!ENDIF # INTRIN
!IFNDEF PLAT
PLAT=$(FORT)\$(OS)-$(ARCH)-$(ABI)$(DEBUG)
!ENDIF # !PLAT
MKFS=obj.mk Makefile arrbio\Makefile blksvd\Makefile dynord\Makefile ksvd2\Makefile faux\Makefile test\Makefile
!INCLUDE obj.mk
LAPACK=mkl_intel_$(ABI)_dll.lib mkl_sequential_dll.lib mkl_core_dll.lib

all: dirs $(LIBS) $(EXES)
	@echo $(PLAT)

help:
	@echo "nmake.exe [FORT=ifort|ifx] [ABI=lp64|ilp64] [NDEBUG=optimization_level] [all|clean|dirs|doc|help]"

dirs:
	-md ..\bin\$(PLAT)
	-md ..\lib\$(PLAT)
	-md ..\obj\$(PLAT)

doc: ..\Doxyfile
	cd .. && rd doc /s /q && doxygen

!INCLUDE arrbio\Makefile
!INCLUDE blksvd\Makefile
!INCLUDE dynord\Makefile
!INCLUDE ksvd2\Makefile
!INCLUDE faux\Makefile
!INCLUDE test\Makefile

clean:
	-rd ..\bin\$(PLAT) /s /q
	-rd ..\lib\$(PLAT) /s /q
	-rd ..\obj\$(PLAT) /s /q
