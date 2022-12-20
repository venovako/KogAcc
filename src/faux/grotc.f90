  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. N)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. N)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  DO I = 1, M
     X = G(I,P)
     Y = G(I,Q)
     G(I,P) = X * W(1,1) + Y * W(2,1)
     G(I,Q) = X * W(1,2) + Y * W(2,2)
  END DO
