  INFO = 0
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  K = 1
  L = 1
  DO I = 1, N-1
     DO J = I+1, N
        W(K,L) = CR_HYPOT(W(J,I), W(I,J))
        P(K,L) = I
        Q(K,L) = J
        K = K + 1
        IF (K .GT. N) THEN
           K = 1
           L = L + 1
        END IF
     END DO
  END DO
