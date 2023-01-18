!>@brief \b ZKSVD2 computes the SVD of a double precision complex 2x2 matrix G as G = U S V^H, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 double precision complex matrix with all components of its elements finite.
!!@param U [OUT]; U is a unitary double precision complex matrix of order two.
!!@param V [OUT]; V is a unitary double precision complex matrix of order two.
!!@param S [OUT]; S' is a double precision real array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite component, then s=-HUGE(s), U=V=I, and s_{11}'=s_{22}'=0.
#ifdef NDEBUG
PURE SUBROUTINE ZKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
#else
SUBROUTINE ZKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, REAL64
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: USE_IEEE_INTRINSIC
#endif
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
#else
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, PARAMETER :: K = REAL64, IERR = -HUGE(0)
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
  INTEGER :: I, J, L, M

#include "hksvd2.F90"
#ifndef NDEBUG
9 FORMAT(2(A,ES25.17E3))
#endif
END SUBROUTINE ZKSVD2
