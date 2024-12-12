!>@brief \b XROTC postmultiplies the columns (p,q) of G by W.
SUBROUTINE XROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = c_long_double
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
#define VL 1
  REAL(KIND=K) :: X(VL)
  REAL(KIND=K) :: Y(VL)
  REAL(KIND=K) :: Z(2,2)
  INTEGER :: I, J
#include "grotc.F90"
END SUBROUTINE XROTC
