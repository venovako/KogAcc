#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > c$1.out
date > c$1.err
for ((I=128;I<=5376;I+=128))
do
	OMP_NUM_THREADS=$1 ${HOME}/Downloads/VecJac/src/tcgesvj-lp64_3q.exe ${I} ${I} c${I} >> c$1.out 2>> c$1.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > c$1.csv
for ((I=128;I<=5376;I+=128))
do
	B=c${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/cerrsvd.exe ${I} ${I} ${B}.G ${B}.UL ${B}.SL ${B}.VL`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/cortho.exe ${B}.UL ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/cortho.exe ${B}.VL ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.SL`
	printf "%4d," $I >> c$1.csv
	echo "${A},${U},${V},${S}" >> c$1.csv
done
unset I
