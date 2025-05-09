!>@brief \b SBUPDATE updates G, U, and V.
SUBROUTINE SBUPDATE(M, B, G, LDG, U, LDU, V, LDV, GB, UB, VB, LDB, NB, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE SBROTC(M, B, G, LDG, P, Q, R, LDB, W, LDW, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, B, LDG, P, Q, LDB, LDW
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,M)
       REAL(KIND=REAL32), INTENT(IN) :: R(LDB,2*B)
       REAL(KIND=REAL32), INTENT(OUT) :: W(LDW,2*B)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SBROTC
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SBROTR(M, B, G, LDG, P, Q, R, LDB, W, LDW, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, B, LDG, P, Q, LDB, LDW
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,M)
       REAL(KIND=REAL32), INTENT(IN) :: R(LDB,2*B)
       REAL(KIND=REAL32), INTENT(OUT) :: W(LDW,B)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE SBROTR
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SCONJT(N, G, LDG, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDG
       REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE SCONJT
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: M, B, LDG, LDU, LDV, LDB, NB, O(2,2*NB)
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,M), U(LDU,M), V(LDV,M), GB(LDB,2*B,NB), UB(LDB,2*B,NB)
  REAL(KIND=K), INTENT(IN) :: VB(LDB,2*B,NB)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J, L
#define BROTC SBROTC
#define BROTR SBROTR
#define CONJT SCONJT
#include "gbupdate.F90"
END SUBROUTINE SBUPDATE
