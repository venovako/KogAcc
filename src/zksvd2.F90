!>@brief \b ZKSVD2 computes the SVD of a double precision complex \f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^H\f$.
!!
!!@param G [IN]; \f$G\f$ is a general \f$2\times 2\f$ double precision complex matrix with all components of its elements finite.
!!@param U [OUT]; \f$U\f$ is a unitary double precision complex matrix of order two.
!!@param V [OUT]; \f$V\f$ is a unitary double precision complex matrix of order two.
!!@param S [OUT]; \f$\Sigma'\f$ is a double precision real array with two elements, \f$\sigma_{11}'\f$ and \f$\sigma_{22}'\f$, both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter \f$s\f$ such that \f$2^{-s}\Sigma'=\Sigma\f$.
!!If \f$G\f$ has a non-finite component, then \f$s=-\mathop{\mathtt{HUGE}}(0)\f$, \f$U=V=I\f$, and \f$\sigma_{11}'=\sigma_{22}'=0\f$.
#ifdef CR_MATH
SUBROUTINE ZKSVD2(G, U, V, S, INFO)
#else
PURE SUBROUTINE ZKSVD2(G, U, V, S, INFO)
#endif
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, PARAMETER :: K = REAL64, IERR = -HUGE(0)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  REAL(KIND=K), PARAMETER :: H = HUGE(ZERO), ROOTH = SQRT(H)
  COMPLEX(KIND=K), PARAMETER :: CZERO = (ZERO,ZERO), CONE = (ONE,ZERO)

  COMPLEX(KIND=K), INTENT(IN) :: G(2,2)
  COMPLEX(KIND=K), INTENT(OUT) :: U(2,2), V(2,2)
  REAL(KIND=K), INTENT(OUT) :: S(2)
  INTEGER, INTENT(OUT) :: INFO

  COMPLEX(KIND=K) :: B(2,2), Z
  REAL(KIND=K) :: A(2,2), X, Y, T
  REAL(KIND=K) :: TANG, SECG, TANF, SECF, TANP, SECP

#ifdef USE_IEEE_INTRINSIC
#include "hksvd2i.f90"
#else
#include "hksvd2.f90"
#endif
END SUBROUTINE ZKSVD2
