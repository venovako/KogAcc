!>@brief \b SROTC postmultiplies the columns (p,q) of G by W.
PURE SUBROUTINE SROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
  IF (MOD(M, 16) .EQ. 0) THEN
     CALL SROTC16(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(M, 8) .EQ. 0) THEN
     CALL SROTC8(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(M, 4) .EQ. 0) THEN
     CALL SROTC4(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(M, 2) .EQ. 0) THEN
     CALL SROTC2(M, N, G, LDG, P, Q, W, INFO)
  ELSE ! the general case
     CALL SROTC1(M, N, G, LDG, P, Q, W, INFO)
  END IF
CONTAINS
  PURE SUBROUTINE SROTC1(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 1
#include "grotc.F90"
  END SUBROUTINE SROTC1
  PURE SUBROUTINE SROTC2(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 2
#include "grotc.F90"
  END SUBROUTINE SROTC2
  PURE SUBROUTINE SROTC4(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 4
#include "grotc.F90"
  END SUBROUTINE SROTC4
  PURE SUBROUTINE SROTC8(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 8
#include "grotc.F90"
  END SUBROUTINE SROTC8
  PURE SUBROUTINE SROTC16(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
    REAL(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 16
#include "grotc.F90"
  END SUBROUTINE SROTC16
END SUBROUTINE SROTC
