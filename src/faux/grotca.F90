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
        IF (.NOT. (ABS(W(I,J)) .LE. HUGE(W(I,J)))) THEN
           INFO = -7
           RETURN
        END IF
        WW(I,J) = REAL(W(I,J), L)
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
        XX(J) = REAL(X(J), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        YY(J) = REAL(Y(J), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J+HL) = XX(J) * WW(1,1) + YY(J) * WW(2,1)
        YY(J+HL) = XX(J) * WW(1,2) + YY(J) * WW(2,2)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        G(I+J-1,P) = REAL(XX(J+HL), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        G(I+J-1,Q) = REAL(YY(J+HL), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J) = REAL(X(J+HL), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        YY(J) = REAL(Y(J+HL), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = 1, HL
        XX(J+HL) = XX(J) * WW(1,1) + YY(J) * WW(2,1)
        YY(J+HL) = XX(J) * WW(1,2) + YY(J) * WW(2,2)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = HL+1, VL
        G(I+J-1,P) = REAL(XX(J), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO J = HL+1, VL
        G(I+J-1,Q) = REAL(YY(J), K)
     END DO
  END DO
