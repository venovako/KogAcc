ifndef COMPILER
COMPILER=gfortran
endif # !COMPILER
ifndef ARCH
ARCH=$(shell uname -m)
endif # !ARCH
ifndef ABI
ifdef ANIMATE
ABI=ilp64
else # !ANIMATE
ABI=lp64
endif # ?ANIMATE
endif # !ABI
ifndef OS
OS=$(shell uname)
endif # !OS
ifndef DEL
DEL=rm -frv
endif # !DEL
ifndef MKD
MKD=mkdir -pv
endif # !MKD
ifndef MOV
MOV=mv -fv
endif # !MOV
