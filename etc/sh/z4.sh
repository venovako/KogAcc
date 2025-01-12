#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > z4.out
date > z4.err
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I >> z4.out
	OMP_NUM_THREADS=64 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/zksvd0.exe 4 ${I} zR${I} >> z4.out 2>> z4.err
done
echo '"N", "TIMEs", "PSTEPS", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > z4.csv
for ((I=128;I<=5376;I+=128))
do
	B=zR${I}
	A=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tgensvd/zerrsvd.exe ${I} ${I} ${B}.G ${B}.U4 ${B}.S4 ${B}.V4`
	U=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/zortho.exe ${B}.U4 ${I} ${I}`
	V=`OMP_NUM_THREADS=64 ${HOME}/Downloads/JACSD/tortho/zortho.exe ${B}.V4 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/dsverr.exe ${I} ${B}.S ${B}.S4`
	printf "%4d," $I >> z4.csv
	echo "${A},${U},${V},${S}" >> z4.csv
done
unset I
