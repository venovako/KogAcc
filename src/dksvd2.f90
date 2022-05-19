!>@brief \b DKSVD2 computes the SVD of a double precision \f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^T\f$.
!!
!!@param G [IN]; \f$G\f$ is a general \f$2\times 2\f$ double precision matrix with finite elements.
!!@param U [OUT]; \f$U\f$ is an orthogonal double precision matrix of order two.
!!@param V [OUT]; \f$V\f$ is an orthogonal double precision matrix of order two.
!!@param S [OUT]; \f$\Sigma'\f$ is a double precision array with two elements, \f$\sigma_{11}'\f$ and \f$\sigma_{22}'\f$, both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter \f$s\f$ such that \f$2^{-s}\Sigma'=\Sigma\f$.
!!If \f$G\f$ has a non-finite element, then \f$s=-\mathop{\mathtt{HUGE}}(0)\f$, \f$U=V=I\f$, and \f$\sigma_{11}'=\sigma_{22}'=0\f$.
SUBROUTINE DKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  INTEGER, PARAMETER :: IERR = -HUGE(0)
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64
  REAL(KIND=REAL64), PARAMETER :: H = HUGE(ZERO), ROOTH = SQRT(H)

  REAL(KIND=REAL64), INTENT(IN) :: G(2,2)
  REAL(KIND=REAL64), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(OUT) :: INFO

  REAL(KIND=REAL64) :: B(2,2), X, Y, Z
  REAL(KIND=REAL64) :: TANG, SECG, TANF, SECF, TANP, SECP

  INCLUDE 'gksvd2.f90'
END SUBROUTINE DKSVD2
