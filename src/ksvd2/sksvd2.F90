!>@brief \b SKSVD2 computes the SVD of a single precision 2x2 matrix G as G = U S V^T, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 single precision matrix with finite elements.
!!@param U [OUT]; U is an orthogonal single precision matrix of order two.
!!@param V [OUT]; V is an orthogonal single precision matrix of order two.
!!@param S [OUT]; S' is a single precision array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [INOUT]; do not set to anything but zero on input unless the effects are understood; on output, the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite element, then s=-HUGE(s), U=V=I, and s_{11}'=s_{22}'=0.
#ifdef NDEBUG
PURE SUBROUTINE SKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
#else
SUBROUTINE SKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL32
  !$ USE OMP_LIB
#endif
#ifdef USE_IEEE_INTRINSIC
#if ((USE_IEEE_INTRINSIC & 3) == 0)
#undef USE_IEEE_INTRINSIC
#elif ((USE_IEEE_INTRINSIC & 3) == 1)
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
#endif
  IMPLICIT NONE

#ifdef USE_IEEE_INTRINSIC
#if ((USE_IEEE_INTRINSIC & 3) == 2)
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION IEEE_FMA(X, Y, Z) BIND(C,NAME='fmaf')
#else
     FUNCTION IEEE_FMA(X, Y, Z) BIND(C,NAME='fmaf')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y, Z
       REAL(KIND=c_float) :: IEEE_FMA
     END FUNCTION IEEE_FMA
  END INTERFACE
#endif
#endif
#ifdef CR_MATH
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
#else
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, PARAMETER :: K = REAL32, IERR = -HUGE(0)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, TWO = 2.0_K
  REAL(KIND=K), PARAMETER :: H = HUGE(ZERO), ROOTH = SQRT(H)

  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(INOUT) :: INFO

  REAL(KIND=K) :: B(2,2), T, X, Y, Z
  REAL(KIND=K) :: TANG, SECG, TANF, SECF, TANP, SECP

#include "gksvd2.F90"
#ifndef NDEBUG
9 FORMAT(2(A,ES16.9E2))
#endif
END SUBROUTINE SKSVD2
