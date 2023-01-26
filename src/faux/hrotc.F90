  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. N)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. N)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  !DIR$ ASSUME_ALIGNED G:64, X:64, Y:64
  DO I = 1, M, VL
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, VL
        X(J) = G(I+J-1,P)
        Y(J) = G(I+J-1,Q)
     END DO
     !DIR$ VECTOR ALIGNED ALWAYS
     DO J = 1, VL
        G(I+J-1,P) = X(J) * W(1,1) + Y(J) * W(2,1)
        G(I+J-1,Q) = X(J) * W(1,2) + Y(J) * W(2,2)
     END DO
  END DO
