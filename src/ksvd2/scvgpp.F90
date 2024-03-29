!>@brief \b SCVGPP postprocess the arguments of SKSVD2 and SLWSV2.
PURE SUBROUTINE SCVGPP(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32
  REAL(KIND=REAL32), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=REAL32), INTENT(INOUT) :: S(2)
  INTEGER, INTENT(INOUT) :: INFO(3)
  REAL(KIND=REAL32) :: T
  LOGICAL :: LND, LNU, LNV
#include "gcvgpp.F90"
END SUBROUTINE SCVGPP
