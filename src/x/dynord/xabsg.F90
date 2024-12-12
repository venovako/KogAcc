!>@brief \b XABSG computes W=|G|.
SUBROUTINE XABSG(M, N, G, LDG, W, LDW, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M, N, LDG, LDW
  REAL(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: W(LDW,N)
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=c_long_double) :: H
  INTEGER :: I, J
#include "gabsg.F90"
END SUBROUTINE XABSG
