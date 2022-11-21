!>@brief \b SPSUM4 sums four non-negative weights in single precision.
PURE SUBROUTINE SPSUM4(W1, W2, W3, W4, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32
  REAL(KIND=REAL32), INTENT(IN) :: W1, W2, W3, W4
  REAL(KIND=REAL32), INTENT(OUT) :: W
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL32) :: T(4)
  INTEGER :: I, J, K
  INCLUDE 'gpsum4.f90'
END SUBROUTINE SPSUM4
