  L = INFO
  INFO = 0
  IF (B .LT. 1) INFO = -4
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (MOD(N, B) .NE. 0) THEN
     INFO = -4
     RETURN
  END IF
  M = N / B
  IF (M .LE. 0) THEN
     INFO = -2
     RETURN
  END IF
  IF (MOD(M, 2) .EQ. 0) THEN
     K = (M / 2) * (M - 1)
  ELSE ! M ood
     K = M * ((M - 1) / 2)
  END IF

  INFO = -1
  !$ IF (L .NE. 0) INFO = INFO - ABS(L)
  CALL NRM2B(N, G, LDG, B, W, INFO)
  IF (INFO .LT. 0) RETURN
  INFO = L
  CALL B2ENC(M, W, D, O, INFO)
  IF (INFO .LT. 0) RETURN
  K = INFO
  INFO = L
  CALL MKDPQ(M, K, D, OD, INFO)
