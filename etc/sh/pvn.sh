#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
echo '"RUN", "COND2G", "UORTHO1", "VORTHO1", "SVDRES1"' > $1$2.out
for ((I=0;I<$3;++I))
do
	printf "%2d," $I >> $1$2.out
	OMP_NUM_THREADS=$4 ${HOME}/Downloads/libpvn/src/pvn_sv2.exe $1 $2 $3 >> $1$2.out
done
unset I
