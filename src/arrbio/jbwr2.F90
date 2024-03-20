!>@brief \b JBWR2 writes a two-dimensional INT64 array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the number of rows of G.
!!@param N [IN]; the number of columns of G.
!!@param G [IN]; an INT64 array to be written.
!!@param LDG [IN]; the leading dimension of G.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE JBWR2(U, M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE JBWR1(U, M, G, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M
       INTEGER(KIND=INT64) :: G(M)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE JBWR1
  END INTERFACE

  INTEGER, INTENT(IN) :: U, M, N, LDG
  INTEGER(KIND=INT64) :: G(LDG,N)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: J

  INFO = 0
  IF (LDG .LT. M) INFO = -5
  IF (N .LT. 0) INFO = -3
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN
  DO J = 1, N
     CALL JBWR1(U, M, G(1,J), INFO)
     IF (INFO .NE. 0) EXIT
  END DO
END SUBROUTINE JBWR2
