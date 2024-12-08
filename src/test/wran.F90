!>@brief WRAN generates a set of pseudorandom extended precision complex 2x2 matrices.
PROGRAM WRAN
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  REAL(KIND=c_long_double), PARAMETER :: ZERO = 0.0_c_long_double
  CHARACTER(LEN=64) :: CLA
  INTEGER :: I, N
  INTEGER(KIND=c_int) :: U, P
  LOGICAL :: UPPER
  INTEGER(KIND=c_int), EXTERNAL :: PVN_RAN_OPEN, PVN_RAN_CLOSE
  REAL(KIND=c_long_double), EXTERNAL :: PVN_RAN_SAFE_L
#define RAN_SAFE PVN_RAN_SAFE_L
#include "hran.F90"
1 FORMAT(A,ES30.21E4)
END PROGRAM WRAN
