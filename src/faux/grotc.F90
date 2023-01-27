  !DIR$ ASSUME_ALIGNED G:64, X:64, Y:64, Z:64
  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. N)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. N)) INFO = -5
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
  IF (Z(1,1) .LT. Z(2,1)) INFO = 1
  IF (Z(2,2) .LT. Z(1,2)) INFO = IOR(INFO, 2)

  Z = W
  SELECT CASE (INFO)
  CASE (0)
     ! should not happen with W orthogonal
     IF ((Z(1,1) .EQ. 0.0_K) .OR. (Z(2,2) .EQ. 0.0_K)) GOTO 1
     Z(2,1) = Z(2,1) / Z(1,1)
     Z(1,2) = Z(1,2) / Z(2,2)
     DO I = 1, M, VL
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           X(J) = G(I+J-1,P)
           Y(J) = G(I+J-1,Q)
        END DO
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           G(I+J-1,P) = (X(J) + Y(J) * Z(2,1)) * Z(1,1)
           G(I+J-1,Q) = (X(J) * Z(1,2) + Y(J)) * Z(2,2)
        END DO
     END DO
  CASE (1)
     ! should not happen with W orthogonal
     IF ((Z(2,1) .EQ. 0.0_K) .OR. (Z(2,2) .EQ. 0.0_K)) GOTO 1
     Z(1,1) = Z(1,1) / Z(2,1)
     Z(1,2) = Z(1,2) / Z(2,2)
     DO I = 1, M, VL
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           X(J) = G(I+J-1,P)
           Y(J) = G(I+J-1,Q)
        END DO
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           G(I+J-1,P) = (X(J) * Z(1,1) + Y(J)) * Z(2,1)
           G(I+J-1,Q) = (X(J) * Z(1,2) + Y(J)) * Z(2,2)
        END DO
     END DO
  CASE (2)
     ! should not happen with W orthogonal
     IF ((Z(1,1) .EQ. 0.0_K) .OR. (Z(1,2) .EQ. 0.0_K)) GOTO 1
     Z(2,1) = Z(2,1) / Z(1,1)
     Z(2,2) = Z(2,2) / Z(1,2)
     DO I = 1, M, VL
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           X(J) = G(I+J-1,P)
           Y(J) = G(I+J-1,Q)
        END DO
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           G(I+J-1,P) = (X(J) + Y(J) * Z(2,1)) * Z(1,1)
           G(I+J-1,Q) = (X(J) + Y(J) * Z(2,2)) * Z(1,2)
        END DO
     END DO
  CASE (3)
     ! should not happen with W orthogonal
     IF ((Z(2,1) .EQ. 0.0_K) .OR. (Z(1,2) .EQ. 0.0_K)) GOTO 1
     Z(1,1) = Z(1,1) / Z(2,1)
     Z(2,2) = Z(2,2) / Z(1,2)
     DO I = 1, M, VL
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           X(J) = G(I+J-1,P)
           Y(J) = G(I+J-1,Q)
        END DO
        !DIR$ VECTOR ALIGNED ALWAYS
        DO J = 1, VL
           G(I+J-1,P) = (X(J) * Z(1,1) + Y(J)) * Z(2,1)
           G(I+J-1,Q) = (X(J) + Y(J) * Z(2,2)) * Z(1,2)
        END DO
     END DO
  CASE DEFAULT
     INFO = -8
     RETURN
  END SELECT
  GOTO 2

1 DO I = 1, M, VL
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, VL
        X(J) = G(I+J-1,P)
        Y(J) = G(I+J-1,Q)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, VL
        G(I+J-1,P) = X(J) * Z(1,1) + Y(J) * Z(2,1)
        G(I+J-1,Q) = X(J) * Z(1,2) + Y(J) * Z(2,2)
     END DO
  END DO
2 CONTINUE
