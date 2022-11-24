!>@brief \b SLWSV2 computes the SVD of an upper triangular single precision \f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^T\f$ by calling the LAPACK's SLASV2 routine.
!!
!!@param G [IN]; \f$G\f$ is an upper triangular \f$2\times 2\f$ single precision matrix.
!!@param U [OUT]; \f$U\f$ is an orthogonal single precision matrix of order two.
!!@param V [OUT]; \f$V\f$ is an orthogonal single precision matrix of order two.
!!@param S [OUT]; \f$\Sigma\f$ is a single precision array with two elements, \f$\sigma_{11}\f$ and \f$\sigma_{22}\f$.
!!@param INFO [OUT]; On success, \f$0\f$; else, if \f$G\f$ is not upper triangular, \f$-\mathop{\mathtt{HUGE}}(0)\f$ (and \f$U=V=I\f$, \f$\sigma_{11}=\sigma_{22}=0\f$).
SUBROUTINE SLWSV2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE

  INTEGER, PARAMETER :: K = REAL32, IERR = -HUGE(0)
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K

  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(OUT) :: INFO

  EXTERNAL :: SLASV2

  U(1,1) = ONE
  U(2,1) = ZERO
  U(1,2) = ZERO
  U(2,2) = ONE
  V(1,1) = ONE
  V(2,1) = ZERO
  V(1,2) = ZERO
  V(2,2) = ONE
  S(1) = ZERO
  S(2) = ZERO

  IF (G(2,1) .EQ. ZERO) THEN
     CALL SLASV2(G(1,1), G(1,2), G(2,2), S(2), S(1), V(2,1), V(1,1), U(2,1), U(1,1))
     U(1,2) = -U(2,1)
     U(2,2) =  U(1,1)
     V(1,2) = -V(2,1)
     V(2,2) =  V(1,1)
     INFO = 0
  ELSE ! G is not upper triangular
     INFO = IERR
  END IF
END SUBROUTINE SLWSV2
