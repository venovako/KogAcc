#!/bin/bash
# !!! has to be run from this directory !!!
if [ -z "${GNU}" ]
then
	cd ../../libpvn/src
	make COMPILER=gcc NDEBUG=3 SAFE=SV2,NRM OPENMP=0 clean
	make COMPILER=gcc NDEBUG=3 SAFE=SV2,NRM OPENMP=0 -j all
	cd ../../KogAcc/src
	make LAPACK=sequential clean all
	cd ../etc
else
	cd ../../libpvn/src
	make COMPILER=gcc COMPILER_SUFFIX=${GNU} NDEBUG=3 SAFE=SV2,NRM OPENMP=0 clean
	make COMPILER=gcc COMPILER_SUFFIX=${GNU} NDEBUG=3 SAFE=SV2,NRM OPENMP=0 -j all
	cd ../../KogAcc/src
	make LAPACK=sequential clean all
	cd ../etc
fi
