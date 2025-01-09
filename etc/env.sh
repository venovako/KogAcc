#!/bin/bash
if [ -n "${KMP_DETERMINISTIC_REDUCTION}" ]
then
	unset KMP_DETERMINISTIC_REDUCTION
fi
export KMP_DETERMINISTIC_REDUCTION=TRUE
if [ -n "${OMP_PLACES}" ]
then
	unset OMP_PLACES
fi
export OMP_PLACES="CORES,THREADS"
if [ -n "${OMP_PROC_BIND}" ]
then
	unset OMP_PROC_BIND
fi
export OMP_PROC_BIND="SPREAD,CLOSE"
if [ -n "${OMP_DYNAMIC}" ]
then
	unset OMP_DYNAMIC
fi
export OMP_DYNAMIC=FALSE
if [ -n "${MKL_DYNAMIC}" ]
then
	unset MKL_DYNAMIC
fi
export MKL_DYNAMIC=FALSE
if [ -n "${OMP_NESTED}" ]
then
	unset OMP_NESTED
fi
export OMP_NESTED=TRUE
if [ -n "${OMP_MAX_ACTIVE_LEVELS}" ]
then
	unset OMP_MAX_ACTIVE_LEVELS
fi
export OMP_MAX_ACTIVE_LEVELS=2
