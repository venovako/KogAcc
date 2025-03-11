#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > sd.out
date > sd.err
for ((I=128;I<=1920;I+=128))
do
	printf "%4d " $I >> sd.out
	OMP_NUM_THREADS=$1 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/sksvdd.exe ${I} s${I} >> sd.out 2>> sd.err
done
echo '"N", "PSTEPS", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > sd.csv
for ((I=128;I<=1920;I+=128))
do
	B=s${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/serrsvd.exe ${I} ${I} ${B}.G ${B}.U3 ${B}.S3 ${B}.V3`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.U3 ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.V3 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.S3`
	printf "%4d," $I >> sd.csv
	echo "${A},${U},${V},${S}" >> sd.csv
done
unset I
