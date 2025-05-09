!>@brief \b DROTC postmultiplies the columns (p,q) of G by W.
PURE SUBROUTINE DROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
  IF (MOD(M, 8) .EQ. 0) THEN
     CALL DROTC8(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(M, 4) .EQ. 0) THEN
     CALL DROTC4(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(M, 2) .EQ. 0) THEN
     CALL DROTC2(M, N, G, LDG, P, Q, W, INFO)
  ELSE ! the general case
     CALL DROTC1(M, N, G, LDG, P, Q, W, INFO)
  END IF
CONTAINS
  PURE SUBROUTINE DROTC1(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 1
#include "grotc.F90"
  END SUBROUTINE DROTC1
  PURE SUBROUTINE DROTC2(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 2
#include "grotc.F90"
  END SUBROUTINE DROTC2
  PURE SUBROUTINE DROTC4(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 4
#include "grotc.F90"
  END SUBROUTINE DROTC4
  PURE SUBROUTINE DROTC8(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 8
#include "grotc.F90"
  END SUBROUTINE DROTC8
END SUBROUTINE DROTC
