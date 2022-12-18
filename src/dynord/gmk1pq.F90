  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF ((K .LT. 0) .OR. (K .GT. 1)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  M = N * (N - 1)
  M_2 = M / 2

  CALL ABSG(G, LDG, W, N, N, N, 0, INFO)
  IF (INFO .NE. 0) RETURN
  CALL MKWPQ(N, W, O, INFO)
  IF (INFO .NE. 0) RETURN
  CALL PQSRT(M_2, W, O, O(M_2+1), W(M_2+1), O(M+1), O(M+M_2+1), INFO)
  IF (INFO .LT. 0) RETURN
  IF (W(1) .EQ. ZERO) THEN
     O(M+1) = 0
     INFO = 0
  ELSE ! not diagonal
     O(M+1) = 1
     INFO = 1
  END IF
