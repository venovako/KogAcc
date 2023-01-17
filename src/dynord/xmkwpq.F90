!>@brief \b XMKWPQ builds the weights W and the corresponding indexes O from the NxN extended precision matrix W.
PURE SUBROUTINE XMKWPQ(N, G, LDG, W, O, INFO)
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10, ONE = 1.0_10
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=10), INTENT(IN) :: G(LDG,N)
  REAL(KIND=10), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
  REAL(KIND=10) :: H
  INTEGER :: I, J, K, L, M, N2
#include "gmkwpq.F90"
END SUBROUTINE XMKWPQ
