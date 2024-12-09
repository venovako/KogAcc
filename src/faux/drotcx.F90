!>@brief \b DROTCX postmultiplies the columns (p,q) of G by W using an (imperfect) emulation of an accurate a*b+c*d operation.
SUBROUTINE DROTCX(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64, L = c_long_double
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
#define VL 8
  REAL(KIND=K) :: X(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: X
  REAL(KIND=K) :: Y(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: Y
  REAL(KIND=L) :: XX(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: XX
  REAL(KIND=L) :: YY(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: YY
  REAL(KIND=L) :: WW(2,2)
  !DIR$ ATTRIBUTES ALIGN: 64:: WW
  INTEGER :: I, J
  !DIR$ ASSUME_ALIGNED G:64, X:64, Y:64, XX:64, YY:64, WW:64
#define HL 4
  J = INFO
  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. N)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. N)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  DO J = 1, 2
     DO I = 1, 2
#ifndef NDEBUG
        IF (.NOT. (ABS(W(I,J)) .LE. HUGE(W(I,J)))) THEN
           INFO = -7
           RETURN
        END IF
#endif
        WW(I,J) = REAL(W(I,J), L)
     END DO
  END DO

  DO I = 1, M, VL
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, VL
        X(J) = G(I+J-1,P)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, VL
        Y(J) = G(I+J-1,Q)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        XX(J) = REAL(X(J), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        YY(J) = REAL(Y(J), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        XX(J+HL) = XX(J) * WW(1,1) + YY(J) * WW(2,1)
        YY(J+HL) = XX(J) * WW(1,2) + YY(J) * WW(2,2)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        G(I+J-1,P) = REAL(XX(J+HL), K)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        G(I+J-1,Q) = REAL(YY(J+HL), K)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        XX(J) = REAL(X(J+HL), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        YY(J) = REAL(Y(J+HL), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, HL
        XX(J+HL) = XX(J) * WW(1,1) + YY(J) * WW(2,1)
        YY(J+HL) = XX(J) * WW(1,2) + YY(J) * WW(2,2)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = HL+1, VL
        G(I+J-1,P) = REAL(XX(J), K)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = HL+1, VL
        G(I+J-1,Q) = REAL(YY(J), K)
     END DO
  END DO
END SUBROUTINE DROTCX
