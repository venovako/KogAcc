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

  CALL ABSG(N, N, G, LDG, W, N, INFO)
  IF (INFO .NE. 0) RETURN
  J = N * N
  DO I = 1, J, N+1
     W(I) = ZERO
  END DO
  W(J) = NORM2(W)
  CALL MKWPQ(N, G, LDG, W, O, INFO)
  IF (INFO .LE. 0) RETURN
  L = MIN(L, INFO)
  CALL PQSRT(PQCMP, M_2, W, O, O(M_2+1), W(M_2+1), O(M+1), O(M+M_2+1), INFO)
  IF (INFO .LT. 0) RETURN

  I = J
  J = M_2 + 1
  W(J) = W(I)
#ifndef NDEBUG
  DO I = J+1, M
     W(I) = ZERO
  END DO
#endif
  DO I = M+1, N*N
     W(I) = MONE
  END DO
#ifndef NDEBUG
  DO I = M+1, 2*M-1
     O(I) = 0
  END DO
#endif
  O(2*M) = INFO

  I = 1
  INFO = 0
  DO WHILE ((INFO .LT. L) .AND. (W(I) .GE. ZERO))
     J = M_2 + I
     IF ((W(M+O(I)) .EQ. MONE) .AND. (W(M+O(J)) .EQ. MONE)) THEN
        INFO = INFO + 1
        W(M+O(I)) = W(I)
        W(M+O(J)) = W(I)
        O(M+INFO) = I
     END IF
     I = I + 1
     IF (I .GT. M_2) EXIT
  END DO
