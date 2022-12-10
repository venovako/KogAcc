!>@brief \b DABSOG computes \f$W=|G|\f$ in parallel for the \f$(P,P),(Q,P),(P,Q),(Q,Q)\f$ blocks if the block size \f$B\ge 1\f$, or for the whole \f$P\times Q\f$ double precision real matrix \f$G\f$ if \f$B=0\f$.
SUBROUTINE DABSOG(G, LDG, W, LDW, P, Q, B, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: LDG, LDW, P, Q, B
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,*)
  REAL(KIND=REAL64), INTENT(OUT) :: W(LDW,*)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J, PB, QB, UP, UQ
#include "gabsog.F90"
END SUBROUTINE DABSOG
