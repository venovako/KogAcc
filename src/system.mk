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
ifeq ($(OS),Linux)
ifeq ($(ARCH),x86_64)
COMPILER=ifx
else # !x86_64
COMPILER=gfortran
endif # ?x86_64
else # !Linux
COMPILER=gfortran
endif # ?Linux
endif # !COMPILER
ifndef DEL
DEL=rm -frv
endif # !DEL
ifndef MKD
MKD=mkdir -pv
endif # !MKD
ifndef MOV
MOV=mv -fv
endif # !MOV
ifeq ($(filter asm,$(MAKECMDGOALS)),asm)
CFLG=-S
OXT=s
else # normal compilation
CFLG=-c
OXT=o
endif # ?asm
