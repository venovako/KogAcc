!IFNDEF ABI
ABI=lp64
!ENDIF # !ABI
!IFNDEF DEL
DEL=rd /S /Q
!ENDIF # !DEL
!IFNDEF MKD
MKD=md
!ENDIF # !MKD
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
FC=ifx.exe
!IFNDEF MARCH
MARCH=Host
!ENDIF # !MARCH
!IFNDEF NDEBUG
NDEBUG=d
!ENDIF # !NDEBUG
FCFLAGS=/nologo /fpp /Qopenmp /standard-semantics /traceback /DNDEBUG=$(NDEBUG) /DCR_MATH=bundled_noerrno /DLAPACK=sequential /I. /MD /O$(NDEBUG) /Qx$(MARCH) /fp:precise /Qfma /Qftz- /Qprec-div /Qprotect-parens /Qopt-report:3 /Qvec-threshold:0
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(FCFLAGS) /4I8 /DMKL_ILP64 /Qmkl-$(ABI):sequential
!ELSE # lp64
FCFLAGS=$(FCFLAGS) /Qmkl:sequential
!ENDIF # ilp64
LDFLAGS=/LIBPATH:..\..\..\..\libpvn\src pvn.lib
!IFNDEF PLAT
PLAT=ifx\Windows-x64-$(ABI)
!ENDIF # !PLAT
MKFS=out.mk Makefile arrbio\Makefile blksvd\Makefile dynord\Makefile ksvd2\Makefile faux\Makefile test\Makefile
!INCLUDE out.mk

all: dirs $(LIBS) $(EXES)
	copy /Y /V ..\..\..\..\libpvn\src\pvn.dll ..\bin\$(PLAT)

help:
	@echo "nmake.exe [MARCH=Host|...] [NDEBUG=d|1|2|3|...] [all|clean|dirs|help]"

dirs:
	-$(MKD) ..\bin\$(PLAT)
	-$(MKD) ..\lib\$(PLAT)
	-$(MKD) ..\obj\$(PLAT)

!INCLUDE arrbio\Makefile
!INCLUDE blksvd\Makefile
!INCLUDE dynord\Makefile
!INCLUDE ksvd2\Makefile
!INCLUDE faux\Makefile
!INCLUDE test\Makefile

clean:
	-$(DEL) ..\bin\$(PLAT)
	-$(DEL) ..\lib\$(PLAT)
	-$(DEL) ..\obj\$(PLAT)
