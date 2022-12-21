!>@brief \b CABSOG computes \f$W=|G|\f$ in parallel.
SUBROUTINE CABSOG(M, N, G, LDG, W, LDW, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
     ! TODO: cr_hypotf might change errno but a copy can be made that does not
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, INTENT(IN) :: M, N, LDG, LDW
  COMPLEX(KIND=REAL32), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL32), INTENT(OUT) :: W(LDW,N)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL32) :: H
  INTEGER :: I, J
#include "habsog.F90"
END SUBROUTINE CABSOG
