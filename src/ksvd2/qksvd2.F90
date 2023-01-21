!>@brief \b QKSVD2 computes the SVD of a quadruple precision 2x2 matrix G as G = U S V^T, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 quadruple precision matrix with finite elements.
!!@param U [OUT]; U is an orthogonal quadruple precision matrix of order two.
!!@param V [OUT]; V is an orthogonal quadruple precision matrix of order two.
!!@param S [OUT]; S' is a quadruple precision array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite element, then s=-HUGE(s), U=V=I, and s_{11}'=s_{22}'=0.
#ifdef NDEBUG
PURE SUBROUTINE QKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#else
SUBROUTINE QKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL128
  !$ USE OMP_LIB
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: USE_IEEE_INTRINSIC
#endif
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  INTEGER, PARAMETER :: K = REAL128, IERR = -HUGE(0)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, TWO = 2.0_K
  REAL(KIND=K), PARAMETER :: H = HUGE(ZERO), ROOTH = SQRT(H)

  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(OUT) :: INFO

  REAL(KIND=K) :: B(2,2), X, Y, Z
  REAL(KIND=K) :: TANG, SECG, TANF, SECF, TANP, SECP

#include "gksvd2.F90"
#ifndef NDEBUG
9 FORMAT(2(A,ES45.36E4))
#endif
END SUBROUTINE QKSVD2
