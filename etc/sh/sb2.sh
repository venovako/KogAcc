#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh new
hostname > sb$1-$2-$4.out
date > sb$1-$2-$4.err
let "T=$3/$4"
echo "$3 threads split to ${T} outer and $4 inner threads."
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I >> sb$1-$2-$4.out
	OMP_NUM_THREADS="${T},$4" ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/sksvd1.exe $1 ${I} $2 s${I} >> sb$1-$2-$4.out 2>> sb$1-$2-$4.err
done
echo '"N", "SVDRES", "UORTHO", "VORTHO", "MINAE", "MAXAE", "AVGAE", "MINRE", "MAXRE", "AVGRE"' > sb$1-$2-$4.csv
for ((I=128;I<=5376;I+=128))
do
	B=s${I}
	A=`OMP_NUM_THREADS=$3 ${HOME}/Downloads/JACSD/tgensvd/serrsvd.exe ${I} ${I} ${B}.G ${B}.U$1 ${B}.S$1 ${B}.V$1`
	U=`OMP_NUM_THREADS=$3 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.U$1 ${I} ${I}`
	V=`OMP_NUM_THREADS=$3 ${HOME}/Downloads/JACSD/tortho/sortho.exe ${B}.V$1 ${I} ${I}`
	S=`${HOME}/Downloads/JACSD/tgensvd/ssverr.exe ${I} ${B}.S ${B}.S$1`
	printf "%4d," $I >> sb$1-$2-$4.csv
	echo "${A},${U},${V},${S}" >> sb$1-$2-$4.csv
done
unset I
