!>@brief YRND generates a set of pseudorandom quadruple precision complex 2x2 matrices.
PROGRAM YRND
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128, ERROR_UNIT
  IMPLICIT NONE
  REAL(KIND=REAL128), PARAMETER :: ZERO = 0.0_REAL128
  INTEGER, ALLOCATABLE :: ISEED(:)
  REAL(KIND=REAL128), ALLOCATABLE :: H(:)
  CHARACTER(LEN=64) :: CLA
  INTEGER :: SSIZE, N, I
  LOGICAL :: UPPER
  REAL(KIND=REAL128) :: T
#include "hrnd.F90"
1 FORMAT(A,ES45.36E4)
END PROGRAM YRND
