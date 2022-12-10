!>@brief \b QABSOG computes \f$W=|G|\f$ in parallel for the \f$(P,P),(Q,P),(P,Q),(Q,Q)\f$ blocks if the block size \f$B\ge 1\f$, or for the whole \f$P\times Q\f$ quadruple precision real matrix \f$G\f$ if \f$B=0\f$.
SUBROUTINE QABSOG(G, LDG, W, LDW, P, Q, B, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: LDG, LDW, P, Q, B
  REAL(KIND=REAL128), INTENT(IN) :: G(LDG,*)
  REAL(KIND=REAL128), INTENT(OUT) :: W(LDW,*)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J, PB, QB, UP, UQ
#include "gabsog.F90"
END SUBROUTINE QABSOG
