#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh 2
hostname > db$1-$2-$4.out
date > db$1-$2-$4.err
let "T=$3/$4"
echo "$3 threads split to ${T} outer and $4 inner threads."
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I >> db$1-$2-$4.out
	OMP_NUM_THREADS="${T},$4" ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/dksvd1.exe $1 ${I} $2 dR${I} >> db$1-$2-$4.out 2>> db$1-$2-$4.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > db$1-$2-$4.csv
for ((I=128;I<=5376;I+=128))
do
	B=dR${I}
	A=`OMP_NUM_THREADS=$3 ${HOME}/Downloads/JACSD/tgensvd/derrsvd.exe ${I} ${I} ${B}.G ${B}.U$1 ${B}.S$1 ${B}.V$1`
	U=`OMP_NUM_THREADS=$3 ${HOME}/Downloads/JACSD/tortho/dortho.exe ${B}.U$1 ${I} ${I}`
	V=`OMP_NUM_THREADS=$3 ${HOME}/Downloads/JACSD/tortho/dortho.exe ${B}.V$1 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/dsverr.exe ${I} ${B}.S ${B}.S$1`
	printf "%4d," $I >> db$1-$2-$4.csv
	echo "${A},${U},${V},${S}" >> db$1-$2-$4.csv
done
unset I
