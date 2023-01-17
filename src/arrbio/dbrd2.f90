!>@brief \b DBRD2 reads a two-dimensional double precision real array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the number of rows of G.
!!@param N [IN]; the number of columns of G.
!!@param G [OUT]; a double precision real array to be read.
!!@param LDG [IN]; the leading dimension of G.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE DBRD2(U, M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE DBRD1(U, M, G, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M
       REAL(KIND=REAL64), INTENT(OUT) :: G(M)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DBRD1
  END INTERFACE

  INTEGER, INTENT(IN) :: U, M, N, LDG
  REAL(KIND=REAL64), INTENT(OUT) :: G(LDG,N)
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
     CALL DBRD1(U, M, G(1,J), INFO)
     IF (INFO .NE. 0) EXIT
  END DO
END SUBROUTINE DBRD2
