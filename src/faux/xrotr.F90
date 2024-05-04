!>@brief \b XROTR premultiplies the rows (p,q) of G by W.
SUBROUTINE XROTR(M, N, G, LDG, P, Q, W, INFO)
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = 10
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(OUT) :: INFO
#define VL 1
  REAL(KIND=K) :: X(VL)
  REAL(KIND=K) :: Y(VL)
  REAL(KIND=K) :: Z(2,2)
  INTEGER :: I, J
#include "grotr.F90"
END SUBROUTINE XROTR
