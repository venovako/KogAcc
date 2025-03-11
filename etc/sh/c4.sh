#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > c4.out
date > c4.err
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I >> c4.out
	OMP_NUM_THREADS=$1 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/cksvd0.exe 4 ${I} c${I} >> c4.out 2>> c4.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > c4.csv
for ((I=128;I<=5376;I+=128))
do
	B=c${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/cerrsvd.exe ${I} ${I} ${B}.G ${B}.U4 ${B}.S4 ${B}.V4`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/cortho.exe ${B}.U4 ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/cortho.exe ${B}.V4 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.S4`
	printf "%4d," $I >> c4.csv
	echo "${A},${U},${V},${S}" >> c4.csv
done
unset I
