#!/bin/bash
for p in 24 53
do
	if [ $p -eq 24 ]
	then
		P=53
	else
		P=113
	fi
	./relerr.wls $p 113 $P 9 # > relerr-$p-$P.txt
done
unset P p
