!>@brief \b XMKWPQ builds the matrices \f$W,P,Q\f$ of weights and indexes, respectively, where their first \f$(N\cdot(N-1))/2\f$ elements are set, from the \f$N\times N\f$ extended precision matrix \f$W\f$.
PURE SUBROUTINE XMKWPQ(N, W, P, Q, INFO)
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  INTEGER, INTENT(IN) :: N
  REAL(KIND=10), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: P(N,N-1), Q(N,N-1), INFO
  INTEGER :: I, J, K, L
#include "gmkwpq.F90"
END SUBROUTINE XMKWPQ
