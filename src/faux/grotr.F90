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

  Z = ABS(W)
#ifndef NDEBUG
  DO J = 1, 2
     DO I = 1, 2
        IF (.NOT. (Z(I,J) .LE. HUGE(Z(I,J)))) THEN
           INFO = -7
           RETURN
        END IF
     END DO
  END DO
#endif
  IF (Z(1,1) .LT. Z(1,2)) INFO = 1
  IF (Z(2,2) .LT. Z(2,1)) INFO = IOR(INFO, 2)

  Z = W
  SELECT CASE (INFO)
  CASE (0)
     IF ((Z(1,1) .EQ. 0.0_K) .OR. (Z(2,2) .EQ. 0.0_K)) GOTO 1
     Z(1,2) = Z(1,2) / Z(1,1)
     Z(2,1) = Z(2,1) / Z(2,2)
     DO J = 1, N, VL
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           X(I) = G(P,J+I-1)
           Y(I) = G(Q,J+I-1)
        END DO
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           G(P,J+I-1) = Z(1,1) * (X(I) + Z(1,2) * Y(I))
           G(Q,J+I-1) = Z(2,2) * (Z(2,1) * X(I) + Y(I))
        END DO
     END DO
  CASE (1)
     IF ((Z(1,2) .EQ. 0.0_K) .OR. (Z(2,2) .EQ. 0.0_K)) GOTO 1
     Z(1,1) = Z(1,1) / Z(1,2)
     Z(2,1) = Z(2,1) / Z(2,2)
     DO J = 1, N, VL
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           X(I) = G(P,J+I-1)
           Y(I) = G(Q,J+I-1)
        END DO
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           G(P,J+I-1) = Z(1,2) * (Z(1,1) * X(I) + Y(I))
           G(Q,J+I-1) = Z(2,2) * (Z(2,1) * X(I) + Y(I))
        END DO
     END DO
  CASE (2)
     IF ((Z(1,1) .EQ. 0.0_K) .OR. (Z(2,1) .EQ. 0.0_K)) GOTO 1
     Z(1,2) = Z(1,2) / Z(1,1)
     Z(2,2) = Z(2,2) / Z(2,1)
     DO J = 1, N, VL
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           X(I) = G(P,J+I-1)
           Y(I) = G(Q,J+I-1)
        END DO
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           G(P,J+I-1) = Z(1,1) * (X(I) + Z(1,2) * Y(I))
           G(Q,J+I-1) = Z(2,1) * (X(I) + Z(2,2) * Y(I))
        END DO
     END DO
  CASE (3)
     IF ((Z(1,2) .EQ. 0.0_K) .OR. (Z(2,1) .EQ. 0.0_K)) GOTO 1
     Z(1,1) = Z(1,1) / Z(1,2)
     Z(2,2) = Z(2,2) / Z(2,1)
     DO J = 1, N, VL
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           X(I) = G(P,J+I-1)
           Y(I) = G(Q,J+I-1)
        END DO
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           G(P,J+I-1) = Z(1,2) * (Z(1,1) * X(I) + Y(I))
           G(Q,J+I-1) = Z(2,1) * (X(I) + Z(2,2) * Y(I))
        END DO
     END DO
  CASE DEFAULT
     INFO = -8
     RETURN
  END SELECT
  GOTO 2

1 DO J = 1, N, VL
     !DIR$ VECTOR ALWAYS
     DO I = 1, VL
        X(I) = G(P,J+I-1)
        Y(I) = G(Q,J+I-1)
     END DO
     !DIR$ VECTOR ALWAYS
     DO I = 1, VL
        G(P,J+I-1) = Z(1,1) * X(I) + Z(1,2) * Y(I)
        G(Q,J+I-1) = Z(2,1) * X(I) + Z(2,2) * Y(I)
     END DO
  END DO
2 CONTINUE
