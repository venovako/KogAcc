!IFNDEF ABI
ABI=lp64
!ENDIF # !ABI
DEL=rd /S /Q
MKD=md
AR=lib.exe
ARFLAGS=/NOLOGO /VERBOSE
!IFNDEF LIBPVN
LIBPVN=..\..\libpvn
!ENDIF # !LIBPVN
!INCLUDE $(LIBPVN)\src\pvn.mk
FC=$(PVN_FC)
!IF "$(ABI)"=="ilp64"
FCFLAGS=$(PVN_FCFLAGS) /4I8 /Qmkl-$(ABI):sequential /DMKL_ILP64
!ELSE # lp64
FCFLAGS=$(PVN_FCFLAGS) /Qmkl:sequential
!ENDIF # ilp64
FCFLAGS=$(FCFLAGS) /DCR_MATH=bundled_noerrno /DLAPACK=sequential $(PVN_CPPFLAGS)
LDFLAGS=$(PVN_LIBS)
!IFNDEF PLAT
PLAT=ifx\Windows-x64-$(ABI)
!ENDIF # !PLAT
!INCLUDE out.mk
MKFS=out.mk Makefile arrbio\Makefile blksvd\Makefile dynord\Makefile ksvd2\Makefile faux\Makefile test\Makefile

all: dirs $(LIBS) $(EXES)
	copy /Y /V ..\..\libpvn\src\*.dll ..\bin\$(PLAT)

help:
	@echo "nmake.exe [LIBPVN=...] [all|clean|dirs|help]"
	@echo "libpvn has to be build beforehand with the CRT=MD build option"

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
