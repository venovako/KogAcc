!>@brief \b SMKWPQ builds the weights W and the corresponding indexes O from the NxN single precision matrix W.
PURE SUBROUTINE SMKWPQ(N, G, LDG, W, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
#ifdef CR_MATH
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif

  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL32), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL32), INTENT(INOUT) :: W(N,N)
  INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
  REAL(KIND=REAL32) :: H
  INTEGER :: I, J, K, L, M, N2
#include "gmkwpq.F90"
END SUBROUTINE SMKWPQ
