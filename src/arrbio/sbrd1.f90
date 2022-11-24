!>@brief \b SBRD1 reads a one-dimensional single precision real array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the length of \f$G\f$.
!!@param G [OUT]; a single precision real array to be read.
!!@param INFO [OUT]; zero on success, \f$-i\f$ if the \f$i\f$th argument had an illegal value, or a positive I/O error code.
SUBROUTINE SBRD1(U, M, G, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  REAL(KIND=REAL32), INTENT(OUT) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  READ (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE SBRD1
