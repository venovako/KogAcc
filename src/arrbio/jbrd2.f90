!>@brief \b JBRD2 reads a two-dimensional INT64 array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the number of rows of \f$G\f$.
!!@param N [IN]; the number of columns of \f$G\f$.
!!@param G [OUT]; an INT64 array to be read.
!!@param LDG [IN]; the leading dimension of \f$G\f$.
!!@param INFO [OUT]; zero on success, \f$-i\f$ if the \f$i\f$th argument had an illegal value, or a positive I/O error code.
SUBROUTINE JBRD2(U, M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE JBRD1(U, M, G, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M
       INTEGER(KIND=INT64), INTENT(OUT) :: G(M)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE JBRD1
  END INTERFACE

  INTEGER, INTENT(IN) :: U, M, N, LDG
  INTEGER(KIND=INT64), INTENT(OUT) :: G(LDG,N)
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
     CALL JBRD1(U, M, G(1,J), INFO)
     IF (INFO .NE. 0) EXIT
  END DO
END SUBROUTINE JBRD2
