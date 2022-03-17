!>@brief \b QKSVD2 computes the SVD of a quadruple precision
!!\f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^T\f$.
!!
!!@param G [IN]; \f$G\f$ is a general \f$2\times 2\f$ quadruple precision matrix with finite elements.
!!@param U [OUT]; \f$U\f$ is an orthogonal quadruple precision matrix of order two.
!!@param V [OUT]; \f$V\f$ is an orthogonal quadruple precision matrix of order two.
!!@param S [OUT]; \f$\Sigma'\f$ is a quadruple precision array with two elements, \f$\sigma_{11}'\f$ and \f$\sigma_{22}'\f$, both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter \f$s\f$ such that \f$2^{-s}\Sigma'=\Sigma\f$.
!!If \f$G\f$ had a non-finite element, then \f$s=-\mathop{\mathtt{HUGE}}(0)\f$, \f$U=V=I\f$, and \f$\sigma_{11}'=\sigma_{22}'=0\f$.
SUBROUTINE QKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE

  INTEGER, PARAMETER :: IERR = -HUGE(0)
  REAL(KIND=REAL128), PARAMETER :: ZERO = 0.0_REAL128, ONE = 1.0_REAL128

  REAL(KIND=REAL128), INTENT(IN) :: G(2,2)
  REAL(KIND=REAL128), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(OUT) :: INFO

  REAL(KIND=REAL128) :: B(2,2), TANG, SECG, X, Y, Z

  INCLUDE 'gksvd2.f90'
END SUBROUTINE QKSVD2
