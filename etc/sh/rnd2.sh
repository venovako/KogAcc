#!/bin/bash
printf "%2d," $4 >> $1rnd2$2.out
printf "%2d," $4 >> $1rnd2$2.err
OMP_NUM_THREADS=64 ${HOME}/Downloads/KogAcc/bin/ifx/Linux-x86_64-lp64/$1rnd2$2.exe $3 >> $1rnd2$2.out 2>> $1rnd2$2.err
