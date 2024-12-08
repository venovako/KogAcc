!>@brief XRND generates a set of pseudorandom extended precision real 2x2 matrices.
PROGRAM XRND
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  IMPLICIT NONE
  REAL(KIND=c_long_double), PARAMETER :: ZERO = 0.0_c_long_double
  INTEGER, ALLOCATABLE :: ISEED(:)
  REAL(KIND=c_long_double), ALLOCATABLE :: H(:)
  CHARACTER(LEN=64) :: CLA
  INTEGER :: SSIZE, N, I
  LOGICAL :: UPPER
  REAL(KIND=c_long_double) :: T
#include "grnd.F90"
1 FORMAT(A,ES30.21E4)
END PROGRAM XRND
