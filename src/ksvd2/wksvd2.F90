!>@brief \b WKSVD2 computes the SVD of an extended precision complex 2x2 matrix G as G = U S V^H.
!!
!!@param G [IN]; G is a general 2x2 extended precision complex matrix with all components of its elements finite.
!!@param U [OUT]; U is a unitary extended precision complex matrix of order two.
!!@param V [OUT]; V is a unitary extended precision complex matrix of order two.
!!@param S [OUT]; S' is a extended precision real array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite component, then s=-HUGE(s), U=V=I, and s_{11}'=s_{22}'=0.
#ifdef NDEBUG
PURE SUBROUTINE WKSVD2(G, U, V, S, INFO)
#else
SUBROUTINE WKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: USE_IEEE_INTRINSIC
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
