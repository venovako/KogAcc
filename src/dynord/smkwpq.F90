!>@brief \b SMKWPQ builds the matrices \f$W,P,Q\f$ of weights and indexes, respectively, where their first \f$(N\cdot(N-1))/2\f$ elements are set, from the \f$N\times N\f$ single precision matrix \f$W\f$.
#ifdef CR_MATH
SUBROUTINE SMKWPQ(N, W, P, Q, INFO)
#else
PURE SUBROUTINE SMKWPQ(N, W, P, Q, INFO)
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL32), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: P(N,N-1), Q(N,N-1), INFO
  INTEGER :: I, J, K, L
#include "gmkwpq.F90"
END SUBROUTINE SMKWPQ
