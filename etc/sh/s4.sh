#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > s4.out
date > s4.err
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I >> s4.out
	OMP_NUM_THREADS=64 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/sksvd0.exe 4 ${I} s${I} >> s4.out 2>> s4.err
done
echo '"N", "TIMEs", "PSTEPS", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > s4.csv
for ((I=128;I<=5376;I+=128))
do
	B=s${I}
	A=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tgensvd/serrsvd.exe ${I} ${I} ${B}.G ${B}.U4 ${B}.S4 ${B}.V4`
	U=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.U4 ${I} ${I}`
	V=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.V4 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.S4`
	printf "%4d," $I >> s4.csv
	echo "${A},${U},${V},${S}" >> s4.csv
done
unset I
