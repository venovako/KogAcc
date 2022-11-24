!>@brief \b XBWR1 writes a one-dimensional extended precision real array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of \f$G\f$.
!!@param G [IN]; an extended precision real array to be written.
!!@param INFO [OUT]; zero on success, \f$-i\f$ if the \f$i\f$th argument had an illegal value, or a positive I/O error code.
SUBROUTINE XBWR1(U, M, G, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  REAL(KIND=10), INTENT(IN) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  WRITE (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE XBWR1
