ifndef ABI
ABI=lp64
endif # !ABI
ARCH=$(shell uname -m)
OS=$(shell uname)
DEL=rm -frv
MKD=mkdir -pv
ifeq ($(filter asm,$(MAKECMDGOALS)),asm)
CFLG=-S
OXT=s
else # normal compilation
CFLG=-c
OXT=o
endif # ?asm
