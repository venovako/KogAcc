!>@brief \b IBWR1 writes a one-dimensional INT32 array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of G.
!!@param G [IN]; an INT32 array to be written.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE IBWR1(U, M, G, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  INTEGER(KIND=INT32) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  WRITE (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE IBWR1
