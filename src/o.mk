OBJS_ARRBIO=\
../obj/$(PLAT)/bfopen.o\
../obj/$(PLAT)/ibrd1.o\
../obj/$(PLAT)/jbrd1.o\
../obj/$(PLAT)/sbrd1.o\
../obj/$(PLAT)/cbrd1.o\
../obj/$(PLAT)/dbrd1.o\
../obj/$(PLAT)/zbrd1.o\
../obj/$(PLAT)/ibwr1.o\
../obj/$(PLAT)/jbwr1.o\
../obj/$(PLAT)/sbwr1.o\
../obj/$(PLAT)/cbwr1.o\
../obj/$(PLAT)/dbwr1.o\
../obj/$(PLAT)/zbwr1.o\
../obj/$(PLAT)/ibrd2.o\
../obj/$(PLAT)/jbrd2.o\
../obj/$(PLAT)/sbrd2.o\
../obj/$(PLAT)/cbrd2.o\
../obj/$(PLAT)/dbrd2.o\
../obj/$(PLAT)/zbrd2.o\
../obj/$(PLAT)/ibwr2.o\
../obj/$(PLAT)/jbwr2.o\
../obj/$(PLAT)/sbwr2.o\
../obj/$(PLAT)/cbwr2.o\
../obj/$(PLAT)/dbwr2.o\
../obj/$(PLAT)/zbwr2.o

OBJS_BLKSVD=\
../obj/$(PLAT)/sbrdg.o\
../obj/$(PLAT)/cbrdg.o\
../obj/$(PLAT)/dbrdg.o\
../obj/$(PLAT)/zbrdg.o\
../obj/$(PLAT)/sbordg.o\
../obj/$(PLAT)/cbordg.o\
../obj/$(PLAT)/dbordg.o\
../obj/$(PLAT)/zbordg.o\
../obj/$(PLAT)/nb2m.o

OBJS_DYNORD=\
../obj/$(PLAT)/sabsg.o\
../obj/$(PLAT)/cabsg.o\
../obj/$(PLAT)/dabsg.o\
../obj/$(PLAT)/zabsg.o\
../obj/$(PLAT)/sabsog.o\
../obj/$(PLAT)/cabsog.o\
../obj/$(PLAT)/dabsog.o\
../obj/$(PLAT)/zabsog.o\
../obj/$(PLAT)/smkwpq.o\
../obj/$(PLAT)/dmkwpq.o\
../obj/$(PLAT)/smk1pq.o\
../obj/$(PLAT)/cmk1pq.o\
../obj/$(PLAT)/dmk1pq.o\
../obj/$(PLAT)/zmk1pq.o\
../obj/$(PLAT)/spqcmp.o\
../obj/$(PLAT)/dpqcmp.o\
../obj/$(PLAT)/spqmrg.o\
../obj/$(PLAT)/dpqmrg.o\
../obj/$(PLAT)/spqsrt.o\
../obj/$(PLAT)/dpqsrt.o\
../obj/$(PLAT)/spqsort.o\
../obj/$(PLAT)/dpqsort.o

OBJS_KSVD2=\
../obj/$(PLAT)/sksvd2.o\
../obj/$(PLAT)/cksvd2.o\
../obj/$(PLAT)/dksvd2.o\
../obj/$(PLAT)/zksvd2.o\
../obj/$(PLAT)/slwsv2.o\
../obj/$(PLAT)/dlwsv2.o

OBJS_FAUX=\
../obj/$(PLAT)/sthalt.o\
../obj/$(PLAT)/slango.o\
../obj/$(PLAT)/clango.o\
../obj/$(PLAT)/dlango.o\
../obj/$(PLAT)/zlango.o\
../obj/$(PLAT)/sosum4.o\
../obj/$(PLAT)/dosum4.o\
../obj/$(PLAT)/srotc.o\
../obj/$(PLAT)/crotc.o\
../obj/$(PLAT)/drotc.o\
../obj/$(PLAT)/zrotc.o\
../obj/$(PLAT)/srotr.o\
../obj/$(PLAT)/crotr.o\
../obj/$(PLAT)/drotr.o\
../obj/$(PLAT)/zrotr.o

EXES_DYNORD=\
../bin/$(PLAT)/spqsort.exe\
../bin/$(PLAT)/dpqsort.exe

EXES_KSVD2=\
../bin/$(PLAT)/sksvd2.exe\
../bin/$(PLAT)/cksvd2.exe\
../bin/$(PLAT)/dksvd2.exe\
../bin/$(PLAT)/zksvd2.exe\
../bin/$(PLAT)/slwsv2.exe\
../bin/$(PLAT)/dlwsv2.exe\
../bin/$(PLAT)/srnd.exe

ifneq ($(COMPILER),nvfortran)
OBJS_ARRBIO += \
../obj/$(PLAT)/qbrd1.o\
../obj/$(PLAT)/ybrd1.o\
../obj/$(PLAT)/qbwr1.o\
../obj/$(PLAT)/ybwr1.o\
../obj/$(PLAT)/qbrd2.o\
../obj/$(PLAT)/ybrd2.o\
../obj/$(PLAT)/qbwr2.o\
../obj/$(PLAT)/ybwr2.o
OBJS_BLKSVD += \
../obj/$(PLAT)/qbrdg.o\
../obj/$(PLAT)/ybrdg.o\
../obj/$(PLAT)/qbordg.o\
../obj/$(PLAT)/ybordg.o
OBJS_DYNORD += \
../obj/$(PLAT)/qabsg.o\
../obj/$(PLAT)/yabsg.o\
../obj/$(PLAT)/qabsog.o\
../obj/$(PLAT)/yabsog.o\
../obj/$(PLAT)/qmkwpq.o\
../obj/$(PLAT)/qmk1pq.o\
../obj/$(PLAT)/ymk1pq.o\
../obj/$(PLAT)/qpqcmp.o\
../obj/$(PLAT)/qpqmrg.o\
../obj/$(PLAT)/qpqsrt.o\
../obj/$(PLAT)/qpqsort.o
OBJS_KSVD2 += \
../obj/$(PLAT)/qksvd2.o\
../obj/$(PLAT)/yksvd2.o
OBJS_FAUX += \
../obj/$(PLAT)/qrotc.o\
../obj/$(PLAT)/yrotc.o\
../obj/$(PLAT)/qrotr.o\
../obj/$(PLAT)/yrotr.o
EXES_DYNORD += \
../bin/$(PLAT)/qpqsort.exe
EXES_KSVD2 += \
../bin/$(PLAT)/qksvd2.exe\
../bin/$(PLAT)/yksvd2.exe
ifeq ($(COMPILER),gfortran)
ifeq ($(findstring 86,$(ARCH)),86)
OBJS_ARRBIO += \
../obj/$(PLAT)/xbrd1.o\
../obj/$(PLAT)/wbrd1.o\
../obj/$(PLAT)/xbwr1.o\
../obj/$(PLAT)/wbwr1.o\
../obj/$(PLAT)/xbrd2.o\
../obj/$(PLAT)/wbrd2.o\
../obj/$(PLAT)/xbwr2.o\
../obj/$(PLAT)/wbwr2.o
OBJS_BLKSVD += \
../obj/$(PLAT)/xbrdg.o\
../obj/$(PLAT)/wbrdg.o\
../obj/$(PLAT)/xbordg.o\
../obj/$(PLAT)/wbordg.o
OBJS_DYNORD += \
../obj/$(PLAT)/xabsg.o\
../obj/$(PLAT)/wabsg.o\
../obj/$(PLAT)/xabsog.o\
../obj/$(PLAT)/wabsog.o\
../obj/$(PLAT)/xmkwpq.o\
../obj/$(PLAT)/xmk1pq.o\
../obj/$(PLAT)/wmk1pq.o\
../obj/$(PLAT)/xpqcmp.o\
../obj/$(PLAT)/xpqmrg.o\
../obj/$(PLAT)/xpqsrt.o\
../obj/$(PLAT)/xpqsort.o
OBJS_KSVD2 += \
../obj/$(PLAT)/xksvd2.o\
../obj/$(PLAT)/wksvd2.o
OBJS_FAUX += \
../obj/$(PLAT)/xrotc.o\
../obj/$(PLAT)/wrotc.o\
../obj/$(PLAT)/xrotr.o\
../obj/$(PLAT)/wrotr.o
EXES_DYNORD += \
../bin/$(PLAT)/xpqsort.exe
EXES_KSVD2 += \
../bin/$(PLAT)/xksvd2.exe\
../bin/$(PLAT)/wksvd2.exe
endif # x86
endif # gfortran
endif # !nvfortran
ifeq ($(findstring DUSE_IEEE_INTRINSIC,$(FCFLAGS)),DUSE_IEEE_INTRINSIC)
INTRIN=i
endif # DUSE_IEEE_INTRINSIC

OBJS=$(OBJS_ARRBIO) $(OBJS_BLKSVD) $(OBJS_DYNORD) $(OBJS_KSVD2) $(OBJS_FAUX)

LIBS=\
../lib/$(PLAT)/libarrbio.a\
../lib/$(PLAT)/libblksvd.a\
../lib/$(PLAT)/libdynord.a\
../lib/$(PLAT)/libksvd2.a\
../lib/$(PLAT)/libfaux.a

EXES=$(EXES_KSVD2) $(EXES_DYNORD)
