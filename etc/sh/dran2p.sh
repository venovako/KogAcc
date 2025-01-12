#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo -n '"P", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > dran2$1p.out
echo -n '"p", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > dran2$1p.err
if [ "$1" = "u" ]
then
	echo ', "UORTHO2", "VORTHO2", "SVDRES2", "MAXREL2", "MINREL2", "U21", "V21", "R21", "M21", "N21", "MAXKs", "AVGKs"' >> dran2up.out
	echo ', "uortho2", "vortho2", "svdres2", "maxrel2", "minrel2", "u21", "v21", "r21", "m21", "n21", "MAXLs", "AVGLs"' >> dran2up.err
else
	echo ', "MAXKs", "AVGKs"' >> dran2$1p.out
	echo ', "MAXLs", "AVGLs"' >> dran2$1p.err
fi
for ((P=$3;P<$4;P+=$5))
do
	${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/dran2$1.exe $2 $P > dran2$1p.out.$P 2> dran2$1p.err.$P &
done
wait
for ((P=$3;P<$4;P+=$5))
do
	printf "%4d," $P >> dran2$1p.out
	cat dran2$1p.out.$P >> dran2$1p.out
	rm dran2$1p.out.$P
	printf "%4d," $P >> dran2$1p.err
	cat dran2$1p.err.$P >> dran2$1p.err
	rm dran2$1p.err.$P
done
unset P
