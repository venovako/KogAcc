!>@brief \b DMKWPQ builds the weights \f$W\f$ and the corresponding indexes \f$O\f$ from the \f$N\times N\f$ double precision matrix \f$W\f$.
#ifdef NDEBUG
PURE SUBROUTINE DMKWPQ(N, G, LDG, W, O, INFO)
#else
SUBROUTINE DMKWPQ(N, G, LDG, W, O, INFO)
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
#else
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL64), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
  REAL(KIND=REAL64) :: H
  INTEGER :: I, J, K, L, M, N2
#include "gmkwpq.F90"
END SUBROUTINE DMKWPQ
