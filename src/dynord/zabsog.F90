!>@brief \b ZABSOG computes \f$W=|G|\f$ in parallel for the \f$(P,P),(Q,P),(P,Q),(Q,Q)\f$ blocks if the block size \f$B\ge 1\f$, or for the whole \f$P\times Q\f$ double precision complex matrix \f$G\f$ if \f$B=0\f$.
SUBROUTINE ZABSOG(G, LDG, W, LDW, P, Q, B, INFO)
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

  INTEGER, INTENT(IN) :: LDG, LDW, P, Q, B
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(LDG,*)
  REAL(KIND=REAL64), INTENT(OUT) :: W(LDW,*)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL64) :: H
  INTEGER :: I, J, PB, QB, UP, UQ
#include "habsog.F90"
END SUBROUTINE ZABSOG
