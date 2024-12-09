!>@brief \b WKERR2 computes the errors in the SVD of G.
SUBROUTINE WKERR2(G, U, V, S, E, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL128
#ifndef NDEBUG
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
#endif
  COMPLEX(KIND=c_long_double), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=c_long_double), INTENT(IN) :: S(2)
  REAL(KIND=K), INTENT(OUT) :: E(3)
  INTEGER, INTENT(IN) :: INFO(3)
  REAL(KIND=K) :: EX(4)
  EXTERNAL :: PVN_WYLJR2
#define YLJR2 PVN_WYLJR2
#include "hkerr2.F90"
END SUBROUTINE WKERR2
