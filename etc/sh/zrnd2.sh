#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo '"RUN", "N", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1", "MAXREL1", "MINREL1"' > zrnd2$1.out
echo '"run", "n", "cond2g", "uortho1", "vortho1", "svdres1", "maxrel1", "minrel1"' > zrnd2$1.err
for ((I=0;I<$3;++I))
do
	/usr/bin/time -f%e ./rnd2.sh z $1 $2 $I
done
unset I
