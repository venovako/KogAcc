!>@brief \b QKSVD2 computes the SVD of a quadruple precision \f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^T\f$.
!!
!!@param G [IN]; \f$G\f$ is a general \f$2\times 2\f$ quadruple precision matrix with finite elements.
!!@param U [OUT]; \f$U\f$ is an orthogonal quadruple precision matrix of order two.
!!@param V [OUT]; \f$V\f$ is an orthogonal quadruple precision matrix of order two.
!!@param S [OUT]; \f$\Sigma'\f$ is a quadruple precision array with two elements, \f$\sigma_{11}'\f$ and \f$\sigma_{22}'\f$, both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter \f$s\f$ such that \f$2^{-s}\Sigma'=\Sigma\f$.
!!If \f$G\f$ has a non-finite element, then \f$s=-\mathop{\mathtt{HUGE}}(0)\f$, \f$U=V=I\f$, and \f$\sigma_{11}'=\sigma_{22}'=0\f$.
#ifdef NDEBUG
PURE SUBROUTINE QKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
#else
SUBROUTINE QKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL128
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
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
  INTEGER :: I

#ifdef USE_IEEE_INTRINSIC
#include "gksvd2i.F90"
#else
#include "gksvd2.F90"
#endif
#ifndef NDEBUG
9 FORMAT(2(A,ES45.36E4))
#endif
END SUBROUTINE QKSVD2
