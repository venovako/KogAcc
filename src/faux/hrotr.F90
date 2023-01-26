  INFO = 0
  IF ((Q .LE. P) .OR. (Q .GT. M)) INFO = -6
  IF ((P .LE. 0) .OR. (P .GE. M)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  !DIR$ ASSUME_ALIGNED G:64, X:64, Y:64
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
