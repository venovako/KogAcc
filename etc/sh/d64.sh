#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > d$1.out
date > d$1.err
for ((I=128;I<=5376;I+=128))
do
	OMP_NUM_THREADS=$1 ${HOME}/Downloads/VecJac/src/tdgesvj-lp64_3q.exe ${I} ${I} dR${I} >> d$1.out 2>> d$1.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > d$1.csv
for ((I=128;I<=5376;I+=128))
do
	B=dR${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/derrsvd.exe ${I} ${I} ${B}.G ${B}.UL ${B}.SL ${B}.VL`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/dortho.exe ${B}.UL ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/dortho.exe ${B}.VL ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/dsverr.exe ${I} ${B}.S ${B}.SL`
	printf "%4d," $I >> d$1.csv
	echo "${A},${U},${V},${S}" >> d$1.csv
done
unset I
