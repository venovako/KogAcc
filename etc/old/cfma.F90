! after CUDA's complex FMA from cuComplex.h
#ifdef USE_IEEE_INTRINSIC
#define CFMA(A,B,C) CMPLX(IEEE_FMA(REAL(A),REAL(B),IEEE_FMA(-AIMAG(A),AIMAG(B),REAL(C))),IEEE_FMA(REAL(A),AIMAG(B),IEEE_FMA(AIMAG(A),REAL(B),AIMAG(C))),K)
#else
#define CFMA(A,B,C) A * B + C
#endif
