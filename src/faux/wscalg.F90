!>@brief \b WSCALG scales G by 2^S.
SUBROUTINE WSCALG(M, N, G, LDG, S, INFO)
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = 10
  INTEGER, INTENT(IN) :: M, N, LDG, S
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J
#include "hscalg.F90"
END SUBROUTINE WSCALG
