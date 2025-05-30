!>@brief \b ZKSVDDX tests the ZKSVDD routine.
PROGRAM ZKSVDDX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64, REAL128, ERROR_UNIT, OUTPUT_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE ZBRDG(M, N, G, LDG, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       COMPLEX(KIND=REAL64), INTENT(OUT) :: G(LDG,M)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZBRDG
  END INTERFACE
  INTERFACE
     SUBROUTINE ZKSVDD(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, D, O, OD, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV, O(2,*)
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
       REAL(KIND=REAL64), INTENT(OUT) :: SV(N), W(*)
       REAL(KIND=REAL128), INTENT(OUT) :: D(*)
       INTEGER, INTENT(OUT) :: OD(2,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZKSVDD
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL64, KK = REAL128
#define BRDG ZBRDG
#define KSVDD ZKSVDD
#define RDINP ZRDINP
#define WROUT ZWROUT
#include "hksvddx.F90"
9 FORMAT(3(ES25.17E3,A))
CONTAINS
#include "zrdinp.F90"
#include "zwrout.F90"
END PROGRAM ZKSVDDX
