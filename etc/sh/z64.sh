#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > z64.out
date > z64.err
for ((I=128;I<=5376;I+=128))
do
	OMP_NUM_THREADS=64 ${HOME}/Downloads/VecJac/src/tzgesvj-lp64_3q.exe ${I} ${I} zR${I} >> z64.out 2>> z64.err
done
echo '"N", "TIMEs", "SWEEPS", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > z64.csv
for ((I=128;I<=5376;I+=128))
do
	B=zR${I}
	A=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tgensvd/zerrsvd.exe ${I} ${I} ${B}.G ${B}.UL ${B}.SL ${B}.VL`
	U=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/zortho.exe ${B}.UL ${I} ${I}`
	V=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/zortho.exe ${B}.VL ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/dsverr.exe ${I} ${B}.S ${B}.SL`
	printf "%4d," $I >> z64.csv
	echo "${A},${U},${V},${S}" >> z64.csv
done
unset I
