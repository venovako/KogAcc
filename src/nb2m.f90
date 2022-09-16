!>@brief \b NB2M computes the minimal \f$M\ge N\f$ such that a square matrix of order \f$N\f$ can have exactly \f$B\f$ rows/columns in a block row/column.
!!
!!@param N [IN]; \f$N\ge 2\f$.
!!@param B [IN]; \f$1\le B<N\f$.
!!@param M [OUT]; \f$M\bmod B=0\f$.
!!@param INFO [OUT]; zero on success or \f$-i\f$ if the \f$i\f$th argument had an illegal value.
SUBROUTINE NB2M(N, B, M, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, B
  INTEGER, INTENT(OUT) :: M, INFO

  INFO = 0
  IF (B .LE. 0) INFO = -2
  IF (N .LE. B) INFO = -1
  IF (INFO .NE. 0) RETURN

  M = MOD(N, B)
  IF (M .EQ. 0) THEN
     M = N
  ELSE ! enlarge M
     M = N + (B - M)
  END IF
END SUBROUTINE NB2M
