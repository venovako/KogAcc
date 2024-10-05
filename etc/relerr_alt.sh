#!/bin/bash
for p in 24 53
do
	if [ ${p} -eq 24 ]
	then
		P=53
	else
		P=113
	fi
	./relerr_alt.wls ${p} 113 ${P} 9 > relerr_${p}_${P}.txt
done
unset P p
