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

  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=REAL32), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL32) :: PARAM(5)

  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  PARAM(1) = -1.0_REAL32
  PARAM(2) = W(1,1)
  PARAM(3) = W(2,1)
  PARAM(4) = W(1,2)
  PARAM(5) = W(2,2)

  CALL SROTM(N, G(P,1), LDG, G(Q,1), LDG, PARAM)
END SUBROUTINE SROTR
