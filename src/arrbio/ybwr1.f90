!>@brief \b YBWR1 writes a one-dimensional quadruple precision complex array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of G.
!!@param G [IN]; a quadruple precision complex array to be written.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE YBWR1(U, M, G, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  COMPLEX(KIND=REAL128), INTENT(IN) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  WRITE (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE YBWR1
