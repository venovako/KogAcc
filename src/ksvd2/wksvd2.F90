!>@brief \b WKSVD2 computes the SVD of an extended precision complex \f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^H\f$.
!!
!!@param G [IN]; \f$G\f$ is a general \f$2\times 2\f$ extended precision complex matrix with all components of its elements finite.
!!@param U [OUT]; \f$U\f$ is a unitary extended precision complex matrix of order two.
!!@param V [OUT]; \f$V\f$ is a unitary extended precision complex matrix of order two.
!!@param S [OUT]; \f$\Sigma'\f$ is a extended precision real array with two elements, \f$\sigma_{11}'\f$ and \f$\sigma_{22}'\f$, both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter \f$s\f$ such that \f$2^{-s}\Sigma'=\Sigma\f$.
!!If \f$G\f$ has a non-finite component, then \f$s=-\mathop{\mathtt{HUGE}}(0)\f$, \f$U=V=I\f$, and \f$\sigma_{11}'=\sigma_{22}'=0\f$.
#ifdef NDEBUG
PURE SUBROUTINE WKSVD2(G, U, V, S, INFO)
#else
SUBROUTINE WKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  INTEGER, PARAMETER :: K = 10, IERR = -HUGE(0)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K, TWO = 2.0_K
  REAL(KIND=K), PARAMETER :: H = HUGE(ZERO), ROOTH = SQRT(H)
  COMPLEX(KIND=K), PARAMETER :: CZERO = (ZERO,ZERO), CONE = (ONE,ZERO)

  COMPLEX(KIND=K), INTENT(IN) :: G(2,2)
  COMPLEX(KIND=K), INTENT(OUT) :: U(2,2), V(2,2)
  REAL(KIND=K), INTENT(OUT) :: S(2)
  INTEGER, INTENT(OUT) :: INFO

  COMPLEX(KIND=K) :: B(2,2), Z
  REAL(KIND=K) :: A(2,2), X, Y, T
  REAL(KIND=K) :: TANG, SECG, TANF, SECF, TANP, SECP

#include "hksvd2.F90"
#ifndef NDEBUG
9 FORMAT(2(A,ES30.21E4))
#endif
END SUBROUTINE WKSVD2
