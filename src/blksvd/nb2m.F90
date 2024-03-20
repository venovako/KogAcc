!>@brief \b NB2M computes the minimal M >= N such that a square matrix of order N can have exactly B rows/columns in a block row/column.
!!
!!@param N [IN]; N >= 2.
!!@param B [IN]; 1 <= B < N.
!!@param M [OUT]; M mod B = 0.
!!@param INFO [OUT]; zero on success or -i if the i-th argument had an illegal value.
PURE SUBROUTINE NB2M(N, B, M, INFO)
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
