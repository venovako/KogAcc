!>@brief \b QCVGPP postprocess the arguments of QKSVD2.
PURE SUBROUTINE QCVGPP(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  REAL(KIND=REAL128), PARAMETER :: ZERO = 0.0_REAL128, ONE = 1.0_REAL128
  REAL(KIND=REAL128), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=REAL128), INTENT(INOUT) :: S(2)
  INTEGER, INTENT(INOUT) :: INFO(3)
  REAL(KIND=REAL128) :: T
  LOGICAL :: LND, LNU, LNV
#include "gcvgpp.F90"
END SUBROUTINE QCVGPP
