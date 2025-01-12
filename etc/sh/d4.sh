#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > d4.out
date > d4.err
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I >> d4.out
	OMP_NUM_THREADS=64 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/dksvd0.exe 4 ${I} dR${I} >> d4.out 2>> d4.err
done
echo '"N", "TIMEs", "PSTEPS", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > d4.csv
for ((I=128;I<=5376;I+=128))
do
	B=dR${I}
	A=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tgensvd/derrsvd.exe ${I} ${I} ${B}.G ${B}.U4 ${B}.S4 ${B}.V4`
	U=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/dortho.exe ${B}.U4 ${I} ${I}`
	V=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/dortho.exe ${B}.V4 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/dsverr.exe ${I} ${B}.S ${B}.S4`
	printf "%4d," $I >> d4.csv
	echo "${A},${U},${V},${S}" >> d4.csv
done
unset I
