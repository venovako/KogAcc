#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
hostname > s0.out
date > s0.err
for ((I=128;I<=5376;I+=128))
do
	OMP_NUM_THREADS=1 ${HOME}/Downloads/VecJac/src/tsgesvj-lp64_3q.exe ${I} ${I} s${I} >> s0.out 2>> s0.err &
	sleep 1
done
wait
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > s0.csv
for ((I=128;I<=5376;I+=128))
do
	B=s${I}
	A=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tgensvd/serrsvd.exe ${I} ${I} ${B}.G ${B}.UL ${B}.SL ${B}.VL`
	U=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.UL ${I} ${I}`
	V=`OMP_NUM_THREADS=$1 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.VL ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.SL`
	printf "%4d," $I >> s0.csv
	echo "${A},${U},${V},${S}" >> s0.csv
done
unset I
