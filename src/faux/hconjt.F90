  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (N .EQ. 0) RETURN

  DO J = 1, N
     DO I = 1, J-1
        T = CONJG(G(I,J))
        G(I,J) = CONJG(G(J,I))
        G(J,I) = T
     END DO
     G(J,J) = CONJG(G(J,J))
  END DO
