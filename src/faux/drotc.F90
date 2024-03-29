!>@brief \b DROTC postmultiplies the columns (p,q) of G by W.
PURE SUBROUTINE DROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  INTERFACE
     PURE SUBROUTINE DROTM(N, DX, INCX, DY, INCY, DPARAM)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, INCX, INCY
       REAL(KIND=REAL64), INTENT(INOUT) :: DX(*), DY(*)
       REAL(KIND=REAL64), INTENT(IN) :: DPARAM(5)
     END SUBROUTINE DROTM
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL64
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
#define VL 8
  REAL(KIND=K) :: X(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: X
  REAL(KIND=K) :: Y(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: Y
  REAL(KIND=K) :: Z(2,2)
  !DIR$ ATTRIBUTES ALIGN: 64:: Z
  REAL(KIND=K) :: PARAM(5)
  INTEGER :: I, J

  I = INFO
  IF (I .EQ. 0) THEN
#include "grotc.F90"
  ELSE ! use BLAS
     INFO = 0
     IF ((Q .LE. P) .OR. (Q .GT. N)) INFO = -6
     IF ((P .LE. 0) .OR. (P .GE. N)) INFO = -5
     IF (LDG .LT. M) INFO = -4
     IF (N .LT. 0) INFO = -2
     IF (M .LT. 0) INFO = -1
     IF (INFO .NE. 0) RETURN
     IF (M .EQ. 0) RETURN
     IF (N .EQ. 0) RETURN

     PARAM(2) = W(1,1)
     PARAM(3) = W(1,2)
     PARAM(4) = W(2,1)
     PARAM(5) = W(2,2)

     IF ((PARAM(2) .EQ. 1.0_K) .AND. (PARAM(5) .EQ. 1.0_K)) THEN
        IF ((PARAM(3) .EQ. 0.0_K) .AND. (PARAM(4) .EQ. 0.0_K)) THEN
           PARAM(1) = -2.0_K
        ELSE ! the non-identity case
           PARAM(1) = 0.0_K
        END IF
     ELSE IF ((PARAM(3) .EQ. -1.0_K) .AND. (PARAM(4) .EQ. 1.0_K)) THEN
        PARAM(1) = 1.0_K
     ELSE ! the general case
        PARAM(1) = -1.0_K
     END IF

     CALL DROTM(M, G(1,P), 1, G(1,Q), 1, PARAM)
  END IF
END SUBROUTINE DROTC
