!>@brief \b XKSVD2 computes the SVD of a extended precision
!!\f$2\times 2\f$ matrix \f$G\f$ as \f$G=U\Sigma V^T\f$.
!!
!!@param G [IN]; \f$G\f$ is a general \f$2\times 2\f$ extended precision matrix with finite elements.
!!@param U [OUT]; \f$U\f$ is an orthogonal extended precision matrix of order two.
!!@param V [OUT]; \f$V\f$ is an orthogonal extended precision matrix of order two.
!!@param S [OUT]; \f$\Sigma'\f$ is a extended precision array with two elements, \f$\sigma_{11}'\f$ and \f$\sigma_{22}'\f$, both non-negative and finite.
!!@param INFO [OUT]; the scaling parameter \f$s\f$ such that \f$2^{-s}\Sigma'=\Sigma\f$.
!!If \f$G\f$ had a non-finite element \f$(i,j)\f$, then \f$s=-\mathop{\mathtt{HUGE}}(0)\f$, \f$U=V=I\f$, and \f$\sigma_{11}'=i,\sigma_{22}'=j\f$.
SUBROUTINE XKSVD2(G, U, V, S, INFO)
  IMPLICIT NONE

  INTEGER, PARAMETER :: IERR = -HUGE(0)
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10, ONE = 1.0_10

  REAL(KIND=10), INTENT(IN) :: G(2,2)
  REAL(KIND=10), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(OUT) :: INFO

  REAL(KIND=10) :: A(2,2), TG, SG

  INFO = 0

  A(2,2) = ABS(G(2,2))
  IF (.NOT. (A(2,2) .LE. HUGE(ZERO))) THEN
     INFO = IERR
     S(1) = REAL(2,10)
     S(2) = REAL(2,10)
  END IF

  A(1,2) = ABS(G(1,2))
  IF (.NOT. (A(1,2) .LE. HUGE(ZERO))) THEN
     INFO = IERR
     S(1) = REAL(1,10)
     S(2) = REAL(2,10)
  END IF

  A(2,1) = ABS(G(2,1))
  IF (.NOT. (A(2,1) .LE. HUGE(ZERO))) THEN
     INFO = IERR
     S(1) = REAL(2,10)
     S(2) = REAL(1,10)
  END IF

  A(1,1) = ABS(G(1,1))
  IF (.NOT. (A(1,1) .LE. HUGE(ZERO))) THEN
     INFO = IERR
     S(1) = REAL(1,10)
     S(2) = REAL(1,10)
  END IF

  IF (INFO .NE. 0) THEN
     U(1,1) = ONE
     U(2,1) = ZERO
     U(1,2) = ZERO
     U(2,2) = ONE
     V(1,1) = ONE
     V(2,1) = ZERO
     V(1,2) = ZERO
     V(2,2) = ONE
     RETURN
  END IF

  INFO = IERR
  IF (A(1,1) .NE. ZERO) INFO = MAX(INFO, EXPONENT(A(1,1)))
  IF (A(2,1) .NE. ZERO) INFO = MAX(INFO, EXPONENT(A(2,1)))
  IF (A(1,2) .NE. ZERO) INFO = MAX(INFO, EXPONENT(A(1,2)))
  IF (A(2,2) .NE. ZERO) INFO = MAX(INFO, EXPONENT(A(2,2)))
  IF (INFO .EQ. IERR) THEN
     INFO = 0
  ELSE ! non-zero A
     INFO = EXPONENT(HUGE(ZERO)) - INFO - 2
  END IF

  IF (INFO .NE. 0) THEN
     A(1,1) = SCALE(G(1,1), INFO)
     A(2,1) = SCALE(G(2,1), INFO)
     A(1,2) = SCALE(G(1,2), INFO)
     A(2,2) = SCALE(G(2,2), INFO)
  ELSE ! no scaling
     A(1,1) = G(1,1)
     A(2,1) = G(2,1)
     A(1,2) = G(1,2)
     A(2,2) = G(2,2)
  END IF

  IF (A(2,1) .EQ. ZERO) THEN
     S(1) = ABS(A(1,1))
  ELSE IF (A(1,1) .EQ. ZERO) THEN
     S(1) = ABS(A(2,1))
  ELSE ! full 1st column
     S(1) = HYPOT(A(1,1), A(2,1))
  END IF

  IF (A(1,2) .EQ. ZERO) THEN
     S(2) = ABS(A(2,2))
  ELSE IF (A(2,2) .EQ. ZERO) THEN
     S(2) = ABS(A(1,2))
  ELSE ! full 2nd column
     S(2) = HYPOT(A(1,2), A(2,2))
  END IF

  IF (S(1) .LT. S(2)) THEN
     TG = A(1,1)
     A(1,1) = A(1,2)
     A(1,2) = TG

     TG = A(2,1)
     A(2,1) = A(2,2)
     A(2,2) = TG

     TG = S(1)
     S(1) = S(2)
     S(2) = TG

     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO
  ELSE ! no column swap
     V(1,1) = ONE
     V(2,1) = ZERO
     V(1,2) = ZERO
     V(2,2) = ONE
  END IF

  U(1,1) = ONE
  U(2,1) = ZERO
  U(1,2) = ZERO
  U(2,2) = ONE

  IF (SIGN(ONE, A(1,1)) .NE. ONE) THEN
     A(1,1) = -A(1,1)
     A(1,2) = -A(1,2)
     U(1,1) = -U(1,1)
     U(1,2) = -U(1,2)
  END IF

  IF (SIGN(ONE, A(2,1)) .NE. ONE) THEN
     A(2,1) = -A(2,1)
     A(2,2) = -A(2,2)
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
  END IF

  IF (A(1,1) .LT. A(2,1)) THEN
     TG = A(1,1)
     A(1,1) = A(2,1)
     A(2,1) = TG

     TG = U(1,1)
     U(1,1) = U(2,1)
     U(2,1) = TG

     TG = A(1,2)
     A(1,2) = A(2,2)
     A(2,2) = TG

     TG = U(1,2)
     U(1,2) = U(2,2)
     U(2,2) = TG
  END IF

  IF (A(1,1) .EQ. ZERO) THEN
     TG = ZERO
  ELSE ! non-zero A
     TG = A(2,1) / A(1,1)
  END IF
  SG = SQRT(ONE + TG * TG)

  A(1,1) = S(1)
  A(2,1) = ZERO
  A(1,2) = (A(1,2) + TG * A(2,2)) / SG
  A(2,2) = (A(2,2) - TG * A(1,2)) / SG
  U(1,1) = (U(1,1) + TG * U(2,1)) / SG
  U(2,1) = (U(2,1) - TG * U(1,1)) / SG
  U(1,2) = (U(1,2) + TG * U(2,2)) / SG
  U(2,2) = (U(2,2) - TG * U(1,2)) / SG

  IF (SIGN(ONE, A(1,2)) .NE. ONE) THEN
     A(1,2) = -A(1,2)
     A(2,2) = -A(2,2)
     V(1,2) = -V(1,2)
     V(2,2) = -V(2,2)
  END IF

  IF (SIGN(ONE, A(2,2)) .NE. ONE) THEN
     A(2,1) = -A(2,1)
     A(2,2) = -A(2,2)
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
  END IF
END SUBROUTINE XKSVD2
