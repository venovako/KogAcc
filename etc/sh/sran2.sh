#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo -n '"RUN", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > sran2$1.out
echo -n '"run", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > sran2$1.err
if [ "$1" = "u" ]
then
	echo ', "UORTHO2", "VORTHO2", "SVDRES2", "MAXREL2", "MINREL2", "U21", "V21", "R21", "M21", "N21", "MAXKs", "AVGKs"' >> sran2u.out
	echo ', "uortho2", "vortho2", "svdres2", "maxrel2", "minrel2", "u21", "v21", "r21", "m21", "n21", "MAXLs", "AVGLs"' >> sran2u.err
else
	echo ', "MAXKs", "AVGKs"' >> sran2$1.out
	echo ', "MAXLs", "AVGLs"' >> sran2$1.err
fi
for ((I=0;I<$3;++I))
do
	${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/sran2$1.exe $2 $3 > sran2$1.out.$I 2> sran2$1.err.$I &
done
wait
for ((I=0;I<$3;++I))
do
	printf "%2d," $I >> sran2$1.out
	cat sran2$1.out.$I >> sran2$1.out
	rm sran2$1.out.$I
	printf "%2d," $I >> sran2$1.err
	cat sran2$1.err.$I >> sran2$1.err
	rm sran2$1.err.$I
done
unset I
