!>@brief \b ZABSG sequentially computes \f$W=|G|\f$.
PURE SUBROUTINE ZABSG(M, N, G, LDG, W, LDW, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
     ! TODO: cr_hypot might change errno but a copy can be made that does not
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, INTENT(IN) :: M, N, LDG, LDW
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL64), INTENT(OUT) :: W(LDW,N)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL64) :: H
  INTEGER :: I, J
#include "habsg.F90"
END SUBROUTINE ZABSG
