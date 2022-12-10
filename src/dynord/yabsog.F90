!>@brief \b YABSOG computes \f$W=|G|\f$ in parallel for the \f$(P,P),(Q,P),(P,Q),(Q,Q)\f$ blocks if the block size \f$B\ge 1\f$, or for the whole \f$P\times Q\f$ quadruple precision complex matrix \f$G\f$ if \f$B=0\f$.
SUBROUTINE YABSOG(G, LDG, W, LDW, P, Q, B, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  INTEGER, INTENT(IN) :: LDG, LDW, P, Q, B
  COMPLEX(KIND=REAL128), INTENT(IN) :: G(LDG,*)
  REAL(KIND=REAL128), INTENT(OUT) :: W(LDW,*)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J, PB, QB, UP, UQ
#include "habsog.F90"
END SUBROUTINE YABSOG
