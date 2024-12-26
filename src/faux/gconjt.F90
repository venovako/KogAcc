  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  DO J = 1, N
     DO I = 1, J-1
        T = G(I,J)
        G(I,J) = G(J,I)
        G(J,I) = T
     END DO
  END DO
