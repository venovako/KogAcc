#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
for ((I=128;I<=5376;I+=128))
do
	${HOME}/Downloads/JACSD/tgensvd/dgensvd.exe R${I}.txt 1 ${I} ${I} dR${I} &
done
wait
unset I
