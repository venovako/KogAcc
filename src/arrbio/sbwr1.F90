!>@brief \b SBWR1 writes a one-dimensional single precision real array to a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of G.
!!@param G [IN]; a single precision real array to be written.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE SBWR1(U, M, G, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  REAL(KIND=REAL32), INTENT(IN) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  WRITE (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE SBWR1
