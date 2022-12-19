  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (K .LT. 0) INFO = -1
  L = N / 2 ! TODO
  IF (K .GT. L) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  IF (K .GT. 0) L = K
  IF (L .EQ. 0) THEN
     W(1) = ZERO
     RETURN
  END IF
  M = N * (N - 1)
  M_2 = M / 2

  CALL ABSG(G, LDG, W, N, N, N, 0, INFO)
  IF (INFO .NE. 0) RETURN
  CALL MKWPQ(N, W, O, INFO)
  IF (INFO .NE. 0) RETURN
  CALL PQSRT(PQCMP, M_2, W, O, O(M_2+1), W(M_2+1), O(M+1), O(M+M_2+1), INFO)
  IF (INFO .LT. 0) RETURN
  O(2*M) = INFO

  I = 1
  INFO = 0
  DO WHILE ((INFO .LT. L) .AND. (W(I) .GT. ZERO))
     J = M_2 + I
     IF ((W(M+O(I)) .EQ. ZERO) .AND. (W(M+O(J)) .EQ. ZERO)) THEN
        INFO = INFO + 1
        W(M+O(I)) = I
        W(M+O(J)) = I
        O(M+INFO) = I
     END IF
     I = I + 1
     IF (I .GT. M_2) EXIT
  END DO

  J = M_2 + 1
  IF (INFO .EQ. 0) THEN
     W(J) = ZERO
  ELSE ! INFO >= 1
     W(J) = W(O(M+INFO))
     DO I = INFO-1, 1, -1
        W(J) = CR_HYPOT(W(J), W(O(M+I)))
     END DO
  END IF
