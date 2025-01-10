  L = INFO
  INFO = 0
#ifndef NDEBUG
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (N .EQ. 0) RETURN

  IF (MOD(N, 2) .EQ. 0) THEN
     INFO = (N / 2) * (N - 1)
  ELSE ! N odd
     INFO = N * ((N - 1) / 2)
  END IF

  X = MONE
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(W,D,INFO) PRIVATE(H,K,P,Q) REDUCTION(MAX:X) IF(L .NE. 0)
  DO K = 1, INFO
     P = O(1,K)
     Q = O(2,K)
     H = CR_HYPOT(CR_HYPOT(W(P,P), W(Q,P)), CR_HYPOT(W(P,Q), W(Q,Q)))
     CALL XENC(D(K), H, P, Q)
     X = MAX(X, D(K))
  END DO
  !$OMP END PARALLEL DO
  D(INFO+1) = X
