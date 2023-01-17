!>@brief \b XBWR2 writes a two-dimensional extended precision real array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the number of rows of \f$G\f$.
!!@param N [IN]; the number of columns of \f$G\f$.
!!@param G [IN]; an extended precision real array to be written.
!!@param LDG [IN]; the leading dimension of \f$G\f$.
!!@param INFO [OUT]; zero on success, \f$-i\f$ if the \f$i\f$th argument had an illegal value, or a positive I/O error code.
SUBROUTINE XBWR2(U, M, N, G, LDG, INFO)
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE XBWR1(U, M, G, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M
       REAL(KIND=10), INTENT(IN) :: G(M)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XBWR1
  END INTERFACE

  INTEGER, INTENT(IN) :: U, M, N, LDG
  REAL(KIND=10), INTENT(IN) :: G(LDG,N)
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
     CALL XBWR1(U, M, G(1,J), INFO)
     IF (INFO .NE. 0) EXIT
  END DO
END SUBROUTINE XBWR2
