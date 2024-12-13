!>@brief \b SBUNPACK unpacks G from GB.
SUBROUTINE SBUNPACK(N, G, LDG, B, GB, LDB, NB, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, LDG, B, LDB, NB, O(2,NB)
  REAL(KIND=REAL32), INTENT(OUT) :: G(LDG,N)
  REAL(KIND=REAL32), INTENT(IN) :: GB(LDB,2*B,NB)
  INTEGER, INTENT(INOUT) :: INFO

  INTEGER :: I, J, K, L, M, P, Q
#include "gbunpack.F90"
END SUBROUTINE SBUNPACK
