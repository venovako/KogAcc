!>@brief \b WABSG computes W=|G|.
SUBROUTINE WABSG(M, N, G, LDG, W, LDW, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  INTEGER, INTENT(IN) :: M, N, LDG, LDW
  COMPLEX(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: W(LDW,N)
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=c_long_double) :: H
  INTEGER :: I, J
#include "habsg.F90"
END SUBROUTINE WABSG
