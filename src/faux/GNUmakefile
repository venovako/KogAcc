../lib/$(PLAT)/libfaux.a: $(OBJS_FAUX) $(MKFS) faux/GNUmakefile
	$(AR) $(ARFLAGS) $@ $(OBJS_FAUX)

../obj/$(PLAT)/sthalt.$(OXT): faux/sthalt.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/sthalt.F90

../obj/$(PLAT)/cmul.$(OXT): faux/cmul.F90 faux/hmul.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/cmul.F90

../obj/$(PLAT)/zmul.$(OXT): faux/zmul.F90 faux/hmul.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/zmul.F90

../obj/$(PLAT)/wmul.$(OXT): faux/wmul.F90 faux/hmul.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/wmul.F90

../obj/$(PLAT)/ymul.$(OXT): faux/ymul.F90 faux/hmul.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/ymul.F90

../obj/$(PLAT)/cfma.$(OXT): faux/cfma.F90 faux/hfma.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/cfma.F90

../obj/$(PLAT)/zfma.$(OXT): faux/zfma.F90 faux/hfma.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/zfma.F90

../obj/$(PLAT)/wfma.$(OXT): faux/wfma.F90 faux/hfma.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/wfma.F90

../obj/$(PLAT)/yfma.$(OXT): faux/yfma.F90 faux/hfma.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/yfma.F90

../obj/$(PLAT)/slango.$(OXT): faux/slango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/slango.F90

../obj/$(PLAT)/clango.$(OXT): faux/clango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/clango.F90

../obj/$(PLAT)/dlango.$(OXT): faux/dlango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/dlango.F90

../obj/$(PLAT)/zlango.$(OXT): faux/zlango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/zlango.F90

../obj/$(PLAT)/qlango.$(OXT): faux/qlango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/qlango.F90

../obj/$(PLAT)/ylango.$(OXT): faux/ylango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/ylango.F90

../obj/$(PLAT)/xlango.$(OXT): faux/xlango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/xlango.F90

../obj/$(PLAT)/wlango.$(OXT): faux/wlango.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/wlango.F90

ifeq ($(findstring s,$(ROTACC)),s)
../obj/$(PLAT)/srotc.$(OXT): ../etc/srotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/srotc.F90
else # !s
../obj/$(PLAT)/srotc.$(OXT): faux/srotc.F90 faux/grotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/srotc.F90
endif # ?s

ifeq ($(findstring c,$(ROTACC)),c)
../obj/$(PLAT)/crotc.$(OXT): ../etc/crotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/crotc.F90
else # !c
../obj/$(PLAT)/crotc.$(OXT): faux/crotc.F90 faux/hrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/crotc.F90
endif # ?c

ifeq ($(findstring d,$(ROTACC)),d)
../obj/$(PLAT)/drotc.$(OXT): ../etc/drotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/drotc.F90
else # !d
../obj/$(PLAT)/drotc.$(OXT): faux/drotc.F90 faux/grotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/drotc.F90
endif # ?d

ifeq ($(findstring z,$(ROTACC)),z)
../obj/$(PLAT)/zrotc.$(OXT): ../etc/zrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/zrotc.F90
else # !z
../obj/$(PLAT)/zrotc.$(OXT): faux/zrotc.F90 faux/hrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/zrotc.F90
endif # ?z

../obj/$(PLAT)/qrotc.$(OXT): faux/qrotc.F90 faux/grotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/qrotc.F90

../obj/$(PLAT)/yrotc.$(OXT): faux/yrotc.F90 faux/hrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/yrotc.F90

ifeq ($(findstring x,$(ROTACC)),x)
../obj/$(PLAT)/xrotc.$(OXT): ../etc/xrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/xrotc.F90
else # !x
../obj/$(PLAT)/xrotc.$(OXT): faux/xrotc.F90 faux/grotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/xrotc.F90
endif # ?x

ifeq ($(findstring w,$(ROTACC)),w)
../obj/$(PLAT)/wrotc.$(OXT): ../etc/wrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/wrotc.F90
else # !w
../obj/$(PLAT)/wrotc.$(OXT): faux/wrotc.F90 faux/hrotc.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/wrotc.F90
endif # ?w

ifeq ($(findstring S,$(ROTACC)),S)
../obj/$(PLAT)/srotr.$(OXT): ../etc/srotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/srotr.F90
else # !S
../obj/$(PLAT)/srotr.$(OXT): faux/srotr.F90 faux/grotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/srotr.F90
endif # ?S

ifeq ($(findstring C,$(ROTACC)),C)
../obj/$(PLAT)/crotr.$(OXT): ../etc/crotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/crotr.F90
else # !C
../obj/$(PLAT)/crotr.$(OXT): faux/crotr.F90 faux/hrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/crotr.F90
endif # ?C

ifeq ($(findstring D,$(ROTACC)),D)
../obj/$(PLAT)/drotr.$(OXT): ../etc/drotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/drotr.F90
else # !D
../obj/$(PLAT)/drotr.$(OXT): faux/drotr.F90 faux/grotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/drotr.F90
endif # ?D

ifeq ($(findstring Z,$(ROTACC)),Z)
../obj/$(PLAT)/zrotr.$(OXT): ../etc/zrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/zrotr.F90
else # !Z
../obj/$(PLAT)/zrotr.$(OXT): faux/zrotr.F90 faux/hrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/zrotr.F90
endif # ?Z

../obj/$(PLAT)/qrotr.$(OXT): faux/qrotr.F90 faux/grotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/qrotr.F90

../obj/$(PLAT)/yrotr.$(OXT): faux/yrotr.F90 faux/hrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/yrotr.F90

ifeq ($(findstring X,$(ROTACC)),X)
../obj/$(PLAT)/xrotr.$(OXT): ../etc/xrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/xrotr.F90
else # !X
../obj/$(PLAT)/xrotr.$(OXT): faux/xrotr.F90 faux/grotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/xrotr.F90
endif # ?X

ifeq ($(findstring W,$(ROTACC)),W)
../obj/$(PLAT)/wrotr.$(OXT): ../etc/wrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../etc/wrotr.F90
else # !W
../obj/$(PLAT)/wrotr.$(OXT): faux/wrotr.F90 faux/hrotr.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/wrotr.F90
endif # ?W

../obj/$(PLAT)/sscalg.$(OXT): faux/sscalg.F90 faux/gscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/sscalg.F90

../obj/$(PLAT)/cscalg.$(OXT): faux/cscalg.F90 faux/hscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/cscalg.F90

../obj/$(PLAT)/dscalg.$(OXT): faux/dscalg.F90 faux/gscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/dscalg.F90

../obj/$(PLAT)/zscalg.$(OXT): faux/zscalg.F90 faux/hscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/zscalg.F90

../obj/$(PLAT)/qscalg.$(OXT): faux/qscalg.F90 faux/gscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/qscalg.F90

../obj/$(PLAT)/yscalg.$(OXT): faux/yscalg.F90 faux/hscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/yscalg.F90

../obj/$(PLAT)/xscalg.$(OXT): faux/xscalg.F90 faux/gscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/xscalg.F90

../obj/$(PLAT)/wscalg.$(OXT): faux/wscalg.F90 faux/hscalg.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/wscalg.F90

../obj/$(PLAT)/jstep.$(OXT): faux/jstep.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/jstep.F90

../obj/$(PLAT)/jsweep.$(OXT): faux/jsweep.F90 $(MKFS) faux/GNUmakefile
	cd ../obj/$(PLAT) && $(FC) $(FCFLAGS) $(CFLG) ../../../src/faux/jsweep.F90
