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

  DO I = 1, M, VL
     !DIR$ VECTOR ALWAYS
     DO J = 1, VL
        X(J) = G(I+J-1,P)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, VL
        Y(J) = G(I+J-1,Q)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J) = CMPLX(REAL(X(J)), AIMAG(X(J)), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        YY(J) = CMPLX(REAL(Y(J)), AIMAG(Y(J)), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J+HL) = XX(J) * WW(1,1) + YY(J) * WW(2,1)
        YY(J+HL) = XX(J) * WW(1,2) + YY(J) * WW(2,2)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        G(I+J-1,P) = CMPLX(REAL(XX(J+HL)), AIMAG(XX(J+HL)), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        G(I+J-1,Q) = CMPLX(REAL(YY(J+HL)), AIMAG(YY(J+HL)), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J) = CMPLX(REAL(X(J+HL)), AIMAG(X(J+HL)), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        YY(J) = CMPLX(REAL(Y(J+HL)), AIMAG(Y(J+HL)), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J+HL) = XX(J) * WW(1,1) + YY(J) * WW(2,1)
        YY(J+HL) = XX(J) * WW(1,2) + YY(J) * WW(2,2)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = HL+1, VL
        G(I+J-1,P) = CMPLX(REAL(XX(J)), AIMAG(XX(J)), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = HL+1, VL
        G(I+J-1,Q) = CMPLX(REAL(YY(J)), AIMAG(YY(J)), K)
     END DO
  END DO
