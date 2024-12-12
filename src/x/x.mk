OBJS_ARRBIO=\
../obj/$(PLAT)/qbrd1.$(OXT)\
../obj/$(PLAT)/ybrd1.$(OXT)\
../obj/$(PLAT)/qbwr1.$(OXT)\
../obj/$(PLAT)/ybwr1.$(OXT)\
../obj/$(PLAT)/qbrd2.$(OXT)\
../obj/$(PLAT)/ybrd2.$(OXT)\
../obj/$(PLAT)/qbwr2.$(OXT)\
../obj/$(PLAT)/ybwr2.$(OXT)

OBJS_BLKSVD=\
../obj/$(PLAT)/qbrdg.$(OXT)\
../obj/$(PLAT)/ybrdg.$(OXT)\
../obj/$(PLAT)/qksvd0.$(OXT)\
../obj/$(PLAT)/yksvd0.$(OXT)

OBJS_DYNORD=\
../obj/$(PLAT)/qabsg.$(OXT)\
../obj/$(PLAT)/yabsg.$(OXT)\
../obj/$(PLAT)/qmkwpq.$(OXT)\
../obj/$(PLAT)/ymkwpq.$(OXT)\
../obj/$(PLAT)/qmk3pq.$(OXT)\
../obj/$(PLAT)/ymk3pq.$(OXT)\
../obj/$(PLAT)/qpqcmp.$(OXT)\
../obj/$(PLAT)/qpqmrg.$(OXT)\
../obj/$(PLAT)/qpqsrt.$(OXT)

OBJS_FAUX=\
../obj/$(PLAT)/ymul.$(OXT)\
../obj/$(PLAT)/yfma.$(OXT)\
../obj/$(PLAT)/qlango.$(OXT)\
../obj/$(PLAT)/ylango.$(OXT)\
../obj/$(PLAT)/qrotc.$(OXT)\
../obj/$(PLAT)/yrotc.$(OXT)\
../obj/$(PLAT)/qrotr.$(OXT)\
../obj/$(PLAT)/yrotr.$(OXT)\
../obj/$(PLAT)/qscalg.$(OXT)\
../obj/$(PLAT)/yscalg.$(OXT)

EXES_BLKSVD=\
../bin/$(PLAT)/qksvd0.exe\
../bin/$(PLAT)/yksvd0.exe

EXES_DYNORD=\
../bin/$(PLAT)/qpqsort.exe

EXES_FAUX=\
../bin/$(PLAT)/sthalt.exe\
../bin/$(PLAT)/jsweep.exe

EXES_TEST=\
../bin/$(PLAT)/qran.exe\
../bin/$(PLAT)/yran.exe\
../bin/$(PLAT)/qrnd.exe\
../bin/$(PLAT)/yrnd.exe\
../bin/$(PLAT)/qran2u.exe\
../bin/$(PLAT)/qran2g.exe\
../bin/$(PLAT)/yran2g.exe\
../bin/$(PLAT)/qrnd2u.exe\
../bin/$(PLAT)/qrnd2g.exe\
../bin/$(PLAT)/yrnd2g.exe

ifeq ($(findstring 86,$(ARCH)),86)
ifeq ($(COMPILER),gfortran)
OBJS_ARRBIO += \
../obj/$(PLAT)/xbrd1.$(OXT)\
../obj/$(PLAT)/wbrd1.$(OXT)\
../obj/$(PLAT)/xbwr1.$(OXT)\
../obj/$(PLAT)/wbwr1.$(OXT)\
../obj/$(PLAT)/xbrd2.$(OXT)\
../obj/$(PLAT)/wbrd2.$(OXT)\
../obj/$(PLAT)/xbwr2.$(OXT)\
../obj/$(PLAT)/wbwr2.$(OXT)
OBJS_BLKSVD += \
../obj/$(PLAT)/xbrdg.$(OXT)\
../obj/$(PLAT)/wbrdg.$(OXT)\
../obj/$(PLAT)/xksvd0.$(OXT)\
../obj/$(PLAT)/wksvd0.$(OXT)
OBJS_DYNORD += \
../obj/$(PLAT)/xabsg.$(OXT)\
../obj/$(PLAT)/wabsg.$(OXT)\
../obj/$(PLAT)/xmkwpq.$(OXT)\
../obj/$(PLAT)/wmkwpq.$(OXT)\
../obj/$(PLAT)/xmk3pq.$(OXT)\
../obj/$(PLAT)/wmk3pq.$(OXT)\
../obj/$(PLAT)/xmkdpq.$(OXT)\
../obj/$(PLAT)/wmkdpq.$(OXT)\
../obj/$(PLAT)/xpqcmp.$(OXT)\
../obj/$(PLAT)/xpqmrg.$(OXT)\
../obj/$(PLAT)/xpqsrt.$(OXT)
OBJS_KSVD2 += \
../obj/$(PLAT)/xksvd2.$(OXT)\
../obj/$(PLAT)/wksvd2.$(OXT)\
../obj/$(PLAT)/xcvgpp.$(OXT)\
../obj/$(PLAT)/wcvgpp.$(OXT)\
../obj/$(PLAT)/xlmsv2.$(OXT)\
../obj/$(PLAT)/xlwsv2.$(OXT)\
../obj/$(PLAT)/xkerr2.$(OXT)\
../obj/$(PLAT)/wkerr2.$(OXT)
OBJS_FAUX += \
../obj/$(PLAT)/wmul.$(OXT)\
../obj/$(PLAT)/wfma.$(OXT)\
../obj/$(PLAT)/xlango.$(OXT)\
../obj/$(PLAT)/wlango.$(OXT)\
../obj/$(PLAT)/xrotc.$(OXT)\
../obj/$(PLAT)/xrotca.$(OXT)\
../obj/$(PLAT)/wrotc.$(OXT)\
../obj/$(PLAT)/wrotca.$(OXT)\
../obj/$(PLAT)/xrotr.$(OXT)\
../obj/$(PLAT)/xrotra.$(OXT)\
../obj/$(PLAT)/wrotr.$(OXT)\
../obj/$(PLAT)/wrotra.$(OXT)\
../obj/$(PLAT)/xscalg.$(OXT)\
../obj/$(PLAT)/wscalg.$(OXT)
EXES_BLKSVD += \
../bin/$(PLAT)/xksvd0.exe\
../bin/$(PLAT)/wksvd0.exe
EXES_DYNORD += \
../bin/$(PLAT)/xpqsort.exe
EXES_KSVD2 += \
../bin/$(PLAT)/xksvd2.exe\
../bin/$(PLAT)/wksvd2.exe\
../bin/$(PLAT)/xlwsv2.exe
EXES_TEST += \
../bin/$(PLAT)/xran.exe\
../bin/$(PLAT)/wran.exe\
../bin/$(PLAT)/xrnd.exe\
../bin/$(PLAT)/wrnd.exe\
../bin/$(PLAT)/xran2u.exe\
../bin/$(PLAT)/xran2g.exe\
../bin/$(PLAT)/wran2g.exe\
../bin/$(PLAT)/xrnd2u.exe\
../bin/$(PLAT)/xrnd2g.exe\
../bin/$(PLAT)/wrnd2g.exe
endif # gfortran
endif # x86
