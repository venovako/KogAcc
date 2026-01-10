#!/bin/bash
# !!! has to be run from this directory !!!
if [ -z "${TGT}" ]
then
	cd ../../libpvn/src
	make COMPILER=icx NDEBUG=3 SAFE=SV2,NRM OPENMP=0 clean
	make COMPILER=icx NDEBUG=3 SAFE=SV2,NRM OPENMP=0 -j all
	cd ../../KogAcc/src
	make LAPACK=sequential clean all
	cd ../etc
else
	cd ../../libpvn/src
	make COMPILER=icx MARCH=${TGT} NDEBUG=3 SAFE=SV2,NRM OPENMP=0 clean
	make COMPILER=icx MARCH=${TGT} NDEBUG=3 SAFE=SV2,NRM OPENMP=0 -j all
	cd ../../KogAcc/src
	make LAPACK=sequential clean all
	cd ../etc
fi
