!>@brief \b SROTR premultiplies the rows (p,q) of G by W.
PURE SUBROUTINE SROTR(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE

  INTERFACE
     PURE SUBROUTINE SROTM(N, SX, INCX, SY, INCY, SPARAM)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, INCX, INCY
       REAL(KIND=REAL32), INTENT(INOUT) :: SX(*), SY(*)
       REAL(KIND=REAL32), INTENT(IN) :: SPARAM(5)
     END SUBROUTINE SROTM
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=K) :: X, Y, PARAM(5)
  INTEGER :: J

  J = INFO
  IF (J .EQ. 0) THEN
     INCLUDE 'grotr.f90'
  ELSE ! use BLAS
     INFO = 0
     IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
     IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
     IF (LDG .LT. M) INFO = -4
     IF (N .LT. 0) INFO = -2
     IF (M .LT. 0) INFO = -1
     IF (INFO .NE. 0) RETURN
     IF (M .EQ. 0) RETURN
     IF (N .EQ. 0) RETURN

     PARAM(2) = W(1,1)
     PARAM(3) = W(2,1)
     PARAM(4) = W(1,2)
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

     CALL SROTM(N, G(P,1), LDG, G(Q,1), LDG, PARAM)
  END IF
END SUBROUTINE SROTR
