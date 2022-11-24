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
FCFLAGS=$(FCFLAGS) /DUSE_IEEE_INTRINSIC $(CR_MATH)
INTRIN=i
!ENDIF # CR_MATH
!IFNDEF PLAT
PLAT=ifort\$(OS)-$(ARCH)-$(ABI)$(DEBUG)
!ENDIF # !PLAT
MKFS=Makefile
OBJS_ARRBIO=..\obj\$(PLAT)\bfopen.obj ..\obj\$(PLAT)\ibrd1.obj ..\obj\$(PLAT)\sbrd1.obj ..\obj\$(PLAT)\cbrd1.obj ..\obj\$(PLAT)\dbrd1.obj ..\obj\$(PLAT)\zbrd1.obj ..\obj\$(PLAT)\qbrd1.obj ..\obj\$(PLAT)\ibwr1.obj ..\obj\$(PLAT)\sbwr1.obj ..\obj\$(PLAT)\cbwr1.obj ..\obj\$(PLAT)\dbwr1.obj ..\obj\$(PLAT)\zbwr1.obj ..\obj\$(PLAT)\qbwr1.obj ..\obj\$(PLAT)\ibrd2.obj ..\obj\$(PLAT)\sbrd2.obj ..\obj\$(PLAT)\cbrd2.obj ..\obj\$(PLAT)\dbrd2.obj ..\obj\$(PLAT)\zbrd2.obj ..\obj\$(PLAT)\qbrd2.obj ..\obj\$(PLAT)\ibwr2.obj ..\obj\$(PLAT)\sbwr2.obj ..\obj\$(PLAT)\cbwr2.obj ..\obj\$(PLAT)\dbwr2.obj ..\obj\$(PLAT)\zbwr2.obj ..\obj\$(PLAT)\qbwr2.obj
OBJS_BLKSVD=..\obj\$(PLAT)\sbrdg.obj ..\obj\$(PLAT)\cbrdg.obj ..\obj\$(PLAT)\dbrdg.obj ..\obj\$(PLAT)\zbrdg.obj ..\obj\$(PLAT)\sbordg.obj ..\obj\$(PLAT)\cbordg.obj ..\obj\$(PLAT)\dbordg.obj ..\obj\$(PLAT)\zbordg.obj ..\obj\$(PLAT)\nb2m.obj
OBJS_DYNORD=..\obj\$(PLAT)\spqcmp.obj ..\obj\$(PLAT)\spqmrg.obj ..\obj\$(PLAT)\spqsrt.obj ..\obj\$(PLAT)\spqsort.obj ..\obj\$(PLAT)\dpqcmp.obj ..\obj\$(PLAT)\dpqmrg.obj ..\obj\$(PLAT)\dpqsrt.obj ..\obj\$(PLAT)\dpqsort.obj ..\obj\$(PLAT)\qpqcmp.obj ..\obj\$(PLAT)\qpqmrg.obj ..\obj\$(PLAT)\qpqsrt.obj ..\obj\$(PLAT)\qpqsort.obj
OBJS_KSVD2=..\obj\$(PLAT)\sksvd2.obj ..\obj\$(PLAT)\cksvd2.obj ..\obj\$(PLAT)\dksvd2.obj ..\obj\$(PLAT)\zksvd2.obj ..\obj\$(PLAT)\qksvd2.obj ..\obj\$(PLAT)\yksvd2.obj ..\obj\$(PLAT)\slwsv2.obj ..\obj\$(PLAT)\dlwsv2.obj
OBJS_FAUX=..\obj\$(PLAT)\sthalt.objj ..\obj\$(PLAT)\slango.obj ..\obj\$(PLAT)\clango.obj ..\obj\$(PLAT)\dlango.obj ..\obj\$(PLAT)\zlango.obj ..\obj\$(PLAT)\sosum4.obj ..\obj\$(PLAT)\dosum4.obj ..\obj\$(PLAT)\dsoffsq.obj ..\obj\$(PLAT)\dcoffsq.obj ..\obj\$(PLAT)\qdoffsq.obj ..\obj\$(PLAT)\qzoffsq.ob ..\obj\$(PLAT)\spsum4.obj ..\obj\$(PLAT)\dpsum4.obj ..\obj\$(PLAT)\qpsum4.obj
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

purge:
	-rd ..\bin /s /q
	-rd ..\lib /s /q
	-rd ..\obj /s /q
