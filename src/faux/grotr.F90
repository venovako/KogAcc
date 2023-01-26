  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  !DIR$ VECTOR ALWAYS
  DO J = 1, N
     X = G(P,J)
     Y = G(Q,J)
     !DIR$ FMA
     G(P,J) = W(1,1) * X + W(1,2) * Y
     !DIR$ FMA
     G(Q,J) = W(2,1) * X + W(2,2) * Y
  END DO
