!>@brief \b SBKSVDD calls SKSVDD for all blocks.
SUBROUTINE SBKSVDD(JS, B2, NB, GB, UB, VB, LDB, SB, WB, LW, DB, LD, OD, OB, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE SKSVDD(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, D, O, OD, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV, O(2,*)
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
       REAL(KIND=REAL32), INTENT(OUT) :: SV(N), W(*)
       REAL(KIND=REAL64), INTENT(OUT) :: D(*)
       INTEGER, INTENT(OUT) :: OD(2,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SKSVDD
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32, KK = REAL64, JOB = 960
  INTEGER, INTENT(IN) :: JS, B2, NB, LDB, LW, LD, OB(2,*)
  REAL(KIND=K), INTENT(INOUT) :: GB(LDB,B2,NB)
  REAL(KIND=K), INTENT(OUT) :: UB(LDB,B2,NB), VB(LDB,B2,NB), SB(B2,NB), WB(LW,NB)
  REAL(KIND=KK), INTENT(OUT) :: DB(LD,NB)
  INTEGER, INTENT(OUT) :: OD(2,B2,NB)
  INTEGER, INTENT(INOUT) :: O(2,NB), INFO
  INTEGER :: I, J
#define KSVDD SKSVDD
#define KKSVDD SKSVDD
#include "gbksvdd.F90"
END SUBROUTINE SBKSVDD
