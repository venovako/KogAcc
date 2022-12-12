!>@brief \b QMKWPQ builds the weights \f$W\f$ and the corresponding indexes \f$O\f$ from the \f$N\times N\f$ quadruple precision matrix \f$W\f$.
PURE SUBROUTINE QMKWPQ(N, W, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE

#define CR_HYPOT HYPOT

  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL128), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
  INTEGER :: I, J, K, L, M, N2
#include "gmkwpq.F90"
END SUBROUTINE QMKWPQ
