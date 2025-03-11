#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > dscalb21-8.out
date > dscalb21-8.err
I=5376
for ((J=1;J<=$1;J*=2))
do
	printf "%4d %2d " $I $J >> dscalb21-8.out
	OMP_NUM_THREADS=$J ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/dksvd1.exe 21 ${I} 8 dR${I} >> dscalb21-8.out 2>> dscalb21-8.err
done
unset J I
