#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > dscal.out
date > dscal.err
I=5376
for ((J=1;J<=$1;J*=2))
do
	printf "%4d %2d " $I $J >> dscal.out
	OMP_NUM_THREADS=$J ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/dksvd0.exe 2 ${I} dR${I} >> dscal.out 2>> dscal.err
done
unset J I
