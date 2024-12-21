  L = INFO
  INFO = 0
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  IF (MOD(N, 2) .EQ. 0) THEN
     M = (N / 2) * (N - 1)
  ELSE ! N odd
     M = N * ((N - 1) / 2)
  END IF

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(W,D,M) PRIVATE(H,K,P,Q) IF(L .NE. 0)
  DO K = 1, M
     P = O(1,K)
     Q = O(2,K)
     H = CR_HYPOT(CR_HYPOT(W(P,P), W(Q,P)), CR_HYPOT(W(P,Q), W(Q,Q)))
     CALL XENC(D(K), H, P, Q)
  END DO
  !$OMP END PARALLEL DO
