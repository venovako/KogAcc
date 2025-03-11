#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > cd.out
date > cd.err
for ((I=128;I<=1920;I+=128))
do
	printf "%4d " $I >> cd.out
	OMP_NUM_THREADS=$1 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/cksvdd.exe ${I} c${I} >> cd.out 2>> cd.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > cd.csv
for ((I=128;I<=1920;I+=128))
do
	B=c${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/cerrsvd.exe ${I} ${I} ${B}.G ${B}.U3 ${B}.S3 ${B}.V3`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/cortho.exe ${B}.U3 ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/cortho.exe ${B}.V3 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.S3`
	printf "%4d," $I >> cd.csv
	echo "${A},${U},${V},${S}" >> cd.csv
done
unset I
