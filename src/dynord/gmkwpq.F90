  INFO = 0
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  K = 1
  L = 1
  M = 1
  N2 = N - 1
  IF (MOD(N2, 2) .EQ. 0) THEN
     N2 = (N2 / 2) * N
  ELSE ! odd
     N2 = N2 * (N / 2)
  END IF
  DO I = 1, N-1
     DO J = I+1, N
        H = CR_HYPOT(W(J,I), W(I,J))
#ifndef NDEBUG
        IF (.NOT. (H .LE. HUGE(H))) THEN
           INFO = -4 - M
           RETURN
        END IF
#endif
        W(K,L) = H
        O(M) = I
        O(M + N2) = J
        K = K + 1
        IF (K .GT. N) THEN
           K = 1
           L = L + 1
        END IF
        M = M + 1
     END DO
  END DO
