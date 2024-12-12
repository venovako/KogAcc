!>@brief \b XMKWPQ builds the weights W and the corresponding indexes O from the NxN extended precision matrix W.
PURE SUBROUTINE XMKWPQ(N, G, LDG, W, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  REAL(KIND=c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, ONE = 1.0_c_long_double
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
  REAL(KIND=c_long_double) :: H
  INTEGER :: I, J, K, L, M, N2
#include "gmkwpq.F90"
END SUBROUTINE XMKWPQ
