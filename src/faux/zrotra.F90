!>@brief \b ZROTRA premultiplies the rows (p,q) of G by W using an imperfect emulation of an accurate a*b+c*d operation.
SUBROUTINE ZROTRA(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64, L = REAL128
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
#define VL 4
  COMPLEX(KIND=K) :: X(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: X
  COMPLEX(KIND=K) :: Y(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: Y
  COMPLEX(KIND=L) :: XX(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: XX
  COMPLEX(KIND=L) :: YY(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: YY
  COMPLEX(KIND=L) :: WW(2,2)
  !DIR$ ATTRIBUTES ALIGN: 64:: WW
  INTEGER :: I, J
  !DIR$ ASSUME_ALIGNED G:64, X:64, Y:64, XX:64, YY:64, WW:64
#define HL 2
  I = INFO
  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  DO J = 1, 2
     DO I = 1, 2
#ifndef NDEBUG
        IF (.NOT. (ABS(REAL(W(I,J))) .LE. HUGE(REAL(W(I,J))))) THEN
           INFO = -7
           RETURN
        END IF
        IF (.NOT. (ABS(AIMAG(W(I,J))) .LE. HUGE(AIMAG(W(I,J))))) THEN
           INFO = -7
           RETURN
        END IF
#endif
        WW(I,J) = CMPLX(REAL(W(I,J)), AIMAG(W(I,J)), L)
     END DO
  END DO

  DO J = 1, N, VL
     DO I = 1, VL
        X(I) = G(P,J+I-1)
     END DO
     DO I = 1, VL
        Y(I) = G(Q,J+I-1)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO I = 1, HL
        XX(I) = CMPLX(REAL(X(I)), AIMAG(X(I)), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO I = 1, HL
        YY(I) = CMPLX(REAL(Y(I)), AIMAG(Y(I)), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO I = 1, HL
        XX(I+HL) = WW(1,1) * XX(I) + WW(1,2) * YY(I)
        YY(I+HL) = WW(2,1) * XX(I) + WW(2,2) * YY(I)
     END DO
     DO I = 1, HL
        G(P,J+I-1) = CMPLX(REAL(XX(I+HL)), AIMAG(XX(I+HL)), K)
     END DO
     DO I = 1, HL
        G(Q,J+I-1) = CMPLX(REAL(YY(I+HL)), AIMAG(YY(I+HL)), K)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO I = 1, HL
        XX(I) = CMPLX(REAL(X(I+HL)), AIMAG(X(I+HL)), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO I = 1, HL
        YY(I) = CMPLX(REAL(Y(I+HL)), AIMAG(Y(I+HL)), L)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO I = 1, HL
        XX(I+HL) = WW(1,1) * XX(I) + WW(1,2) * YY(I)
        YY(I+HL) = WW(2,1) * XX(I) + WW(2,2) * YY(I)
     END DO
     DO I = HL+1, VL
        G(P,J+I-1) = CMPLX(REAL(XX(I)), AIMAG(XX(I)), K)
     END DO
     DO I = HL+1, VL
        G(Q,J+I-1) = CMPLX(REAL(YY(I)), AIMAG(YY(I)), K)
     END DO
  END DO
END SUBROUTINE ZROTRA
