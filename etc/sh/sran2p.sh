#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo -n '"P", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > sran2$1p.out
echo -n '"p", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > sran2$1p.err
if [ "$1" = "u" ]
then
	echo ', "UORTHO2", "VORTHO2", "SVDRES2", "MAXREL2", "MINREL2", "U21", "V21", "R21", "M21", "N21", "MAXKs", "AVGKs"' >> sran2up.out
	echo ', "uortho2", "vortho2", "svdres2", "maxrel2", "minrel2", "u21", "v21", "r21", "m21", "n21", "MAXLs", "AVGLs"' >> sran2up.err
else
	echo ', "MAXKs", "AVGKs"' >> sran2$1p.out
	echo ', "MAXLs", "AVGLs"' >> sran2$1p.err
fi
for ((P=$3;P<$4;P+=$5))
do
	${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/sran2$1.exe $2 $P > sran2$1p.out.$P 2> sran2$1p.err.$P &
done
wait
for ((P=$3;P<$4;P+=$5))
do
	printf "%3d," $P >> sran2$1p.out
	cat sran2$1p.out.$P >> sran2$1p.out
	rm sran2$1p.out.$P
	printf "%3d," $P >> sran2$1p.err
	cat sran2$1p.err.$P >> sran2$1p.err
	rm sran2$1p.err.$P
done
unset P
