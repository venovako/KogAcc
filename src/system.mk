ifndef ARCH
ARCH=$(shell uname -m)
endif # !ARCH
ifndef ABI
ABI=lp64
endif # !ABI
ifndef OS
OS=$(shell uname)
endif # !OS
ifndef COMPILER
COMPILER=gfortran
endif # !COMPILER
ifndef DEL
DEL=rm -frv
endif # !DEL
ifndef MKD
MKD=mkdir -pv
endif # !MKD
ifeq ($(filter asm,$(MAKECMDGOALS)),asm)
CFLG=-S
OXT=s
else # normal compilation
CFLG=-c
OXT=o
endif # ?asm
