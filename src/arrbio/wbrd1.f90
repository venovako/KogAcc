!>@brief \b WBRD1 reads a one-dimensional extended precision complex array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of \f$G\f$.
!!@param G [OUT]; an extended precision complex array to be read.
!!@param INFO [OUT]; zero on success, \f$-i\f$ if the \f$i\f$th argument had an illegal value, or a positive I/O error code.
SUBROUTINE WBRD1(U, M, G, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  COMPLEX(KIND=10), INTENT(OUT) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  READ (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE WBRD1
