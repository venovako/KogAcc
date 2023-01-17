!>@brief \b YBRD1 reads a one-dimensional quadruple precision complex array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of G.
!!@param G [OUT]; a quadruple precision complex array to be read.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE YBRD1(U, M, G, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  COMPLEX(KIND=REAL128), INTENT(OUT) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  READ (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE YBRD1
