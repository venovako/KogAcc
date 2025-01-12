#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo -n '"RUN", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > drnd2$1.out
echo -n '"run", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > drnd2$1.err
if [ "$1" = "u" ]
then
	echo ', "UORTHO2", "VORTHO2", "SVDRES2", "MAXREL2", "MINREL2", "U21", "V21", "R21", "M21", "N21", "MAXKs", "AVGKs"' >> drnd2u.out
	echo ', "uortho2", "vortho2", "svdres2", "maxrel2", "minrel2", "u21", "v21", "r21", "m21", "n21", "MAXLs", "AVGLs"' >> drnd2u.err
else
	echo ', "MAXKs", "AVGKs"' >> drnd2$1.out
	echo ', "MAXLs", "AVGLs"' >> drnd2$1.err
fi
for ((I=0;I<$3;++I))
do
	/usr/bin/time -f%e ./rnd2.sh d $1 $2 $I
done
unset I
