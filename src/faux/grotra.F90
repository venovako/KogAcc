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

  I = INFO
  INFO = 0
#ifndef NDEBUG
  IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
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

  DO J = 1, N, VL
     DO I = 1, VL
        X(I) = G(P,J+I-1)
     END DO
     DO I = 1, VL
        Y(I) = G(Q,J+I-1)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, HL
        XX(I) = REAL(X(I), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, HL
        YY(I) = REAL(Y(I), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, HL
        XX(I+HL) = WW(1,1) * XX(I) + WW(1,2) * YY(I)
        YY(I+HL) = WW(2,1) * XX(I) + WW(2,2) * YY(I)
     END DO
     DO I = 1, HL
        G(P,J+I-1) = REAL(XX(I+HL), K)
     END DO
     DO I = 1, HL
        G(Q,J+I-1) = REAL(YY(I+HL), K)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, HL
        XX(I) = REAL(X(I+HL), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, HL
        YY(I) = REAL(Y(I+HL), L)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, HL
        XX(I+HL) = WW(1,1) * XX(I) + WW(1,2) * YY(I)
        YY(I+HL) = WW(2,1) * XX(I) + WW(2,2) * YY(I)
     END DO
     DO I = HL+1, VL
        G(P,J+I-1) = REAL(XX(I), K)
     END DO
     DO I = HL+1, VL
        G(Q,J+I-1) = REAL(YY(I), K)
     END DO
  END DO
