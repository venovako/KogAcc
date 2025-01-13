#!/bin/bash
source ${HOME}/Downloads/KogAcc/etc/env.sh
for ((I=128;I<=5376;I+=128))
do
	printf "%4d " $I
	${HOME}/Downloads/JACSD/tgensvd/dgentxt.exe $I -23 0 | shuf > R$I.txt
	echo "done"
done
unset I
