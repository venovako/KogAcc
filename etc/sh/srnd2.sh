#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo -n '"RUN", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > srnd2$1.out
echo -n '"run", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > srnd2$1.err
if [ "$1" = "u" ]
then
	echo ', "UORTHO2", "VORTHO2", "SVDRES2", "MAXREL2", "MINREL2", "U21", "V21", "R21", "M21", "N21", "MAXKs", "AVGKs"' >> srnd2u.out
	echo ', "uortho2", "vortho2", "svdres2", "maxrel2", "minrel2", "u21", "v21", "r21", "m21", "n21", "MAXLs", "AVGLs"' >> srnd2u.err
else
	echo ', "MAXKs", "AVGKs"' >> srnd2$1.out
	echo ', "MAXLs", "AVGLs"' >> srnd2$1.err
fi
for ((I=0;I<$3;++I))
do
	/usr/bin/time -f%e ./rnd2.sh s $1 $2 $I
done
unset I
