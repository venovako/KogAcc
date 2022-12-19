!>@brief \b DMKWPQ builds the weights \f$W\f$ and the corresponding indexes \f$O\f$ from the \f$N\times N\f$ double precision matrix \f$W\f$.
PURE SUBROUTINE DMKWPQ(N, W, O, INFO)
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

  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL64), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
  REAL(KIND=REAL64) :: H
  INTEGER :: I, J, K, L, M, N2
#include "gmkwpq.F90"
END SUBROUTINE DMKWPQ
