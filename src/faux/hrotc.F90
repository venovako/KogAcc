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

  IF (J .EQ. 0) THEN
     DO I = 1, M, VL
        !DIR$ VECTOR ALWAYS
        DO J = 1, VL
           X(J) = G(I+J-1,P)
           Y(J) = G(I+J-1,Q)
        END DO
        !DIR$ VECTOR ALWAYS
        DO J = 1, VL
           G(I+J-1,P) = X(J) * W(1,1) + Y(J) * W(2,1)
           G(I+J-1,Q) = X(J) * W(1,2) + Y(J) * W(2,2)
        END DO
     END DO
  ELSE ! J .NE. 0
     J = 0
     IF (CR_HYPOT(REAL(W(1,1)), AIMAG(W(1,1))) .LT. CR_HYPOT(REAL(W(2,1)), AIMAG(W(2,1)))) J = 1
     IF (CR_HYPOT(REAL(W(1,2)), AIMAG(W(1,2))) .LT. CR_HYPOT(REAL(W(2,2)), AIMAG(W(2,2)))) J = IOR(J, 2)
     ! DO I = 1, M
     !    X(1) = G(I,P)
     !    Y(1) = G(I,Q)
     !    G(I,P) = X(1) * W(1,1) + Y(1) * W(2,1)
     !    G(I,Q) = X(1) * W(1,2) + Y(1) * W(2,2)
     ! END DO
     SELECT CASE (J)
     CASE (0)
        DO I = 1, M
           X(1) = G(I,P)
           Y(1) = G(I,Q)
           G(I,P) = FMA(X(1), W(1,1), MUL(Y(1), W(2,1)))
           G(I,Q) = FMA(X(1), W(1,2), MUL(Y(1), W(2,2)))
        END DO
     CASE (1)
        DO I = 1, M
           X(1) = G(I,P)
           Y(1) = G(I,Q)
           G(I,P) = FMA(Y(1), W(2,1), MUL(X(1), W(1,1)))
           G(I,Q) = FMA(X(1), W(1,2), MUL(Y(1), W(2,2)))
        END DO
     CASE (2)
        DO I = 1, M
           X(1) = G(I,P)
           Y(1) = G(I,Q)
           G(I,P) = FMA(X(1), W(1,1), MUL(Y(1), W(2,1)))
           G(I,Q) = FMA(Y(1), W(2,2), MUL(X(1), W(1,2)))
        END DO
     CASE (3)
        DO I = 1, M
           X(1) = G(I,P)
           Y(1) = G(I,Q)
           G(I,P) = FMA(Y(1), W(2,1), MUL(X(1), W(1,1)))
           G(I,Q) = FMA(Y(1), W(2,2), MUL(X(1), W(1,2)))
        END DO
     CASE DEFAULT
        INFO = J
     END SELECT
  END IF
