!>@brief \b DMKBPQ computes OD from G w.r.t. B.
SUBROUTINE DMKBPQ(M, G, LDG, B, W, D, O, OD, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE DNRM2B(M, G, LDG, B, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, LDG, B
       REAL(KIND=REAL64), INTENT(IN) :: G(LDG,M)
       REAL(KIND=REAL64), INTENT(OUT) :: W(M/B,M/B)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DNRM2B
  END INTERFACE
  INTERFACE
     SUBROUTINE DB2ENC(N, W, D, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, O(2,*)
       REAL(KIND=REAL64), INTENT(IN) :: W(N,N)
       REAL(KIND=REAL128), INTENT(OUT) :: D(*)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DB2ENC
  END INTERFACE
  INTERFACE
     SUBROUTINE DMKDPQ(N, M, D, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, M
       REAL(KIND=REAL128), INTENT(INOUT) :: D(M+1)
       INTEGER, INTENT(OUT) :: O(2,N/2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE DMKDPQ
  END INTERFACE

  INTEGER, INTENT(IN) :: M, LDG, B, O(2,*)
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,M)
  REAL(KIND=REAL64), INTENT(OUT) :: W(M/B,M/B)
  REAL(KIND=REAL128), INTENT(OUT) :: D(*)
  INTEGER, INTENT(OUT) :: OD(2,*)
  INTEGER, INTENT(INOUT) :: INFO

  INTEGER :: K, L, N
#define NRM2B DNRM2B
#define B2ENC DB2ENC
#define MKDPQ DMKDPQ
#include "gmkbpq.F90"
END SUBROUTINE DMKBPQ
