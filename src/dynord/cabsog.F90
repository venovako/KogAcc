!>@brief \b CABSOG computes \f$W=|G|\f$ in parallel for the \f$(P,P),(Q,P),(P,Q),(Q,Q)\f$ blocks if the block size \f$B\ge 1\f$, or for the whole \f$P\times Q\f$ single precision complex matrix \f$G\f$ if \f$B=0\f$.
SUBROUTINE CABSOG(G, LDG, W, LDW, P, Q, B, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, INTENT(IN) :: LDG, LDW, P, Q, B
  COMPLEX(KIND=REAL32), INTENT(IN) :: G(LDG,*)
  REAL(KIND=REAL32), INTENT(OUT) :: W(LDW,*)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J, PB, QB, UP, UQ
#include "habsog.F90"
END SUBROUTINE CABSOG
