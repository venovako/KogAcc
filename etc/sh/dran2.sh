#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo -n '"RUN", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > dran2$1.out
echo -n '"run", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > dran2$1.err
if [ "$1" = "u" ]
then
	echo ', "UORTHO2", "VORTHO2", "SVDRES2", "MAXREL2", "MINREL2", "U21", "V21", "R21", "M21", "N21", "MAXKs", "AVGKs"' >> dran2u.out
	echo ', "uortho2", "vortho2", "svdres2", "maxrel2", "minrel2", "u21", "v21", "r21", "m21", "n21", "MAXLs", "AVGLs"' >> dran2u.err
else
	echo ', "MAXKs", "AVGKs"' >> dran2$1.out
	echo ', "MAXLs", "AVGLs"' >> dran2$1.err
fi
for ((I=0;I<$3;++I))
do
	${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/dran2$1.exe $2 $3 > dran2$1.out.$I 2> dran2$1.err.$I &
done
wait
for ((I=0;I<$3;++I))
do
	printf "%2d," $I >> dran2$1.out
	cat dran2$1.out.$I >> dran2$1.out
	rm dran2$1.out.$I
	printf "%2d," $I >> dran2$1.err
	cat dran2$1.err.$I >> dran2$1.err
	rm dran2$1.err.$I
done
unset I
