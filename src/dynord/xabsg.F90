!>@brief \b XABSG sequentially computes \f$W=|G|\f$ for the \f$(P,P),(Q,P),(P,Q),(Q,Q)\f$ blocks if the block size \f$B\ge 1\f$, or for the whole \f$P\times Q\f$ extended precision real matrix \f$G\f$ if \f$B=0\f$.
PURE SUBROUTINE XABSG(G, LDG, W, LDW, P, Q, B, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: LDG, LDW, P, Q, B
  REAL(KIND=10), INTENT(IN) :: G(LDG,*)
  REAL(KIND=10), INTENT(OUT) :: W(LDW,*)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=10) :: H
  INTEGER :: I, J, PB, QB, UP, UQ
#include "gabsg.F90"
END SUBROUTINE XABSG
