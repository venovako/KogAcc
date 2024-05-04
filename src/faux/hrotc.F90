  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. N)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. N)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  J = 0
  IF (CR_HYPOT(REAL(W(1,1)), AIMAG(W(1,1))) .LT. CR_HYPOT(REAL(W(2,1)), AIMAG(W(2,1)))) J = 1
  IF (CR_HYPOT(REAL(W(1,2)), AIMAG(W(1,2))) .LT. CR_HYPOT(REAL(W(2,2)), AIMAG(W(2,2)))) J = IOR(J, 2)

  ! DO I = 1, M
  !    X = G(I,P)
  !    Y = G(I,Q)
  !    G(I,P) = X * W(1,1) + Y * W(2,1)
  !    G(I,Q) = X * W(1,2) + Y * W(2,2)
  ! END DO

  SELECT CASE (J)
  CASE (0)
     DO I = 1, M
        X = G(I,P)
        Y = G(I,Q)
        G(I,P) = FMA(X, W(1,1), MUL(Y, W(2,1)))
        G(I,Q) = FMA(X, W(1,2), MUL(Y, W(2,2)))
     END DO
  CASE (1)
     DO I = 1, M
        X = G(I,P)
        Y = G(I,Q)
        G(I,P) = FMA(Y, W(2,1), MUL(X, W(1,1)))
        G(I,Q) = FMA(X, W(1,2), MUL(Y, W(2,2)))
     END DO
  CASE (2)
     DO I = 1, M
        X = G(I,P)
        Y = G(I,Q)
        G(I,P) = FMA(X, W(1,1), MUL(Y, W(2,1)))
        G(I,Q) = FMA(Y, W(2,2), MUL(X, W(1,2)))
     END DO
  CASE (3)
     DO I = 1, M
        X = G(I,P)
        Y = G(I,Q)
        G(I,P) = FMA(Y, W(2,1), MUL(X, W(1,1)))
        G(I,Q) = FMA(Y, W(2,2), MUL(X, W(1,2)))
     END DO
  CASE DEFAULT
     INFO = J
  END SELECT
