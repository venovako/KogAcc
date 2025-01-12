#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo '"RUN", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > zran2$1.out
echo '"run", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > zran2$1.err
for ((I=0;I<$3;++I))
do
	${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/zran2$1.exe $2 $3 > zran2$1.out.$I 2> zran2$1.err.$I &
done
wait
for ((I=0;I<$3;++I))
do
	printf "%2d," $I >> zran2$1.out
	cat zran2$1.out.$I >> zran2$1.out
	rm zran2$1.out.$I
	printf "%2d," $I >> zran2$1.err
	cat zran2$1.err.$I >> zran2$1.err
	rm zran2$1.err.$I
done
unset I
