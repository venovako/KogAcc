  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  I = 0
  IF (CR_HYPOT(REAL(W(1,1)), AIMAG(W(1,1))) .LT. CR_HYPOT(REAL(W(1,2)), AIMAG(W(1,2)))) I = IOR(I, 1)
  IF (CR_HYPOT(REAL(W(2,1)), AIMAG(W(2,1))) .LT. CR_HYPOT(REAL(W(2,2)), AIMAG(W(2,2)))) I = IOR(I, 2)

  ! DO J = 1, N
  !    X = G(P,J)
  !    Y = G(Q,J)
  !    G(P,J) = W(1,1) * X + W(1,2) * Y
  !    G(Q,J) = W(2,1) * X + W(2,2) * Y
  ! END DO

  SELECT CASE (I)
  CASE (0)
     DO J = 1, N
        X = G(P,J)
        Y = G(Q,J)
        G(P,J) = FMA(W(1,1), X, MUL(W(1,2), Y))
        G(Q,J) = FMA(W(2,1), X, MUL(W(2,2), Y))
     END DO
  CASE (1)
     DO J = 1, N
        X = G(P,J)
        Y = G(Q,J)
        G(P,J) = FMA(W(1,2), Y, MUL(W(1,1), X))
        G(Q,J) = FMA(W(2,1), X, MUL(W(2,2), Y))
     END DO
  CASE (2)
     DO J = 1, N
        X = G(P,J)
        Y = G(Q,J)
        G(P,J) = FMA(W(1,1), X, MUL(W(1,2), Y))
        G(Q,J) = FMA(W(2,2), Y, MUL(W(2,1), X))
     END DO
  CASE (3)
     DO J = 1, N
        X = G(P,J)
        Y = G(Q,J)
        G(P,J) = FMA(W(1,2), Y, MUL(W(1,1), X))
        G(Q,J) = FMA(W(2,2), Y, MUL(W(2,1), X))
     END DO
  CASE DEFAULT
     INFO = I
  END SELECT
