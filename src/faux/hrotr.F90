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

  IF (I .EQ. 0) THEN
     DO J = 1, N, VL
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           X(I) = G(P,J+I-1)
           Y(I) = G(Q,J+I-1)
        END DO
        !DIR$ VECTOR ALWAYS
        DO I = 1, VL
           G(P,J+I-1) = W(1,1) * X(I) + W(1,2) * Y(I)
           G(Q,J+I-1) = W(2,1) * X(I) + W(2,2) * Y(I)
        END DO
     END DO
  ELSE ! I .NE. 0
     I = 0
     IF (CR_HYPOT(REAL(W(1,1)), AIMAG(W(1,1))) .LT. CR_HYPOT(REAL(W(1,2)), AIMAG(W(1,2)))) I = 1
     IF (CR_HYPOT(REAL(W(2,1)), AIMAG(W(2,1))) .LT. CR_HYPOT(REAL(W(2,2)), AIMAG(W(2,2)))) I = IOR(I, 2)
     ! DO J = 1, N
     !    X(1) = G(P,J)
     !    Y(1) = G(Q,J)
     !    G(P,J) = W(1,1) * X(1) + W(1,2) * Y(1)
     !    G(Q,J) = W(2,1) * X(1) + W(2,2) * Y(1)
     ! END DO
     SELECT CASE (I)
     CASE (0)
        DO J = 1, N
           X(1) = G(P,J)
           Y(1) = G(Q,J)
           G(P,J) = FMA(W(1,1), X(1), MUL(W(1,2), Y(1)))
           G(Q,J) = FMA(W(2,1), X(1), MUL(W(2,2), Y(1)))
        END DO
     CASE (1)
        DO J = 1, N
           X(1) = G(P,J)
           Y(1) = G(Q,J)
           G(P,J) = FMA(W(1,2), Y(1), MUL(W(1,1), X(1)))
           G(Q,J) = FMA(W(2,1), X(1), MUL(W(2,2), Y(1)))
        END DO
     CASE (2)
        DO J = 1, N
           X(1) = G(P,J)
           Y(1) = G(Q,J)
           G(P,J) = FMA(W(1,1), X(1), MUL(W(1,2), Y(1)))
           G(Q,J) = FMA(W(2,2), Y(1), MUL(W(2,1), X(1)))
        END DO
     CASE (3)
        DO J = 1, N
           X(1) = G(P,J)
           Y(1) = G(Q,J)
           G(P,J) = FMA(W(1,2), Y(1), MUL(W(1,1), X(1)))
           G(Q,J) = FMA(W(2,2), Y(1), MUL(W(2,1), X(1)))
        END DO
     CASE DEFAULT
        INFO = I
     END SELECT
  END IF
