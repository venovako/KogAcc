!>@brief \b WCVGPP postprocess the arguments of WKSVD2.
PURE SUBROUTINE WCVGPP(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  COMPLEX(KIND=c_long_double), PARAMETER :: ZERO = (0.0_c_long_double,0.0_c_long_double), ONE = (1.0_c_long_double,0.0_c_long_double)
  COMPLEX(KIND=c_long_double), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=c_long_double), INTENT(INOUT) :: S(2)
  INTEGER, INTENT(INOUT) :: INFO(3)
  REAL(KIND=c_long_double) :: T
  LOGICAL :: LND, LNU, LNV
#include "hcvgpp.F90"
END SUBROUTINE WCVGPP
