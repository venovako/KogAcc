!>@brief CRND generates a set of pseudorandom single precision complex 2x2 matrices.
PROGRAM CRND
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, ERROR_UNIT
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32
  INTEGER, ALLOCATABLE :: ISEED(:)
  REAL(KIND=REAL32), ALLOCATABLE :: H(:)
  CHARACTER(LEN=64) :: CLA
  INTEGER :: SSIZE, N, I
  LOGICAL :: UPPER
  REAL(KIND=REAL32) :: T
  INCLUDE 'hrnd.f90'
1 FORMAT(A,ES16.9E2)
END PROGRAM CRND
