#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > s$1.out
date > s$1.err
for ((I=128;I<=5376;I+=128))
do
	OMP_NUM_THREADS=$1 ${HOME}/Downloads/VecJac/src/tsgesvj-lp64_3q.exe ${I} ${I} s${I} >> s$1.out 2>> s$1.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > s$1.csv
for ((I=128;I<=5376;I+=128))
do
	B=s${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/serrsvd.exe ${I} ${I} ${B}.G ${B}.UL ${B}.SL ${B}.VL`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.UL ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.VL ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.SL`
	printf "%4d," $I >> s$1.csv
	echo "${A},${U},${V},${S}" >> s$1.csv
done
unset I
