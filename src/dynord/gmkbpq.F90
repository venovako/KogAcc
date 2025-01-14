  L = INFO
  INFO = 0
#ifndef NDEBUG
  IF (B .LT. 1) INFO = -4
  IF (LDG .LT. M) INFO = -3
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (M .EQ. 0) RETURN
#ifndef NDEBUG
  IF (MOD(M, B) .NE. 0) THEN
     INFO = -4
     RETURN
  END IF
#endif
  N = M / B
#ifndef NDEBUG
  IF (N .LE. 0) THEN
     INFO = -2
     RETURN
  END IF
#endif
  IF (MOD(N, 2) .EQ. 0) THEN
     K = (N / 2) * (N - 1)
  ELSE ! N ood
     K = N * ((N - 1) / 2)
  END IF

  INFO = -1
  !$ IF (L .NE. 0) INFO = INFO - ABS(L)
  CALL NRM2B(M, G, LDG, B, W, INFO)
  IF (INFO .LT. 0) RETURN
  INFO = L
  CALL B2ENC(N, W, D, O, INFO)
  IF (INFO .LT. 0) RETURN
  K = INFO
  INFO = -L - 1
  CALL MKDPQ(N, K, D, OD, INFO)
