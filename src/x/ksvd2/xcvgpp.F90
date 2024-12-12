!>@brief \b XCVGPP postprocess the arguments of XKSVD2.
PURE SUBROUTINE XCVGPP(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  REAL(KIND=c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, ONE = 1.0_c_long_double
  REAL(KIND=c_long_double), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=c_long_double), INTENT(INOUT) :: S(2)
  INTEGER, INTENT(INOUT) :: INFO(3)
  REAL(KIND=c_long_double) :: T
  LOGICAL :: LND, LNU, LNV
#include "gcvgpp.F90"
END SUBROUTINE XCVGPP
