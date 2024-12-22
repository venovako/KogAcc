  IF (MOD(N, 2) .EQ. 0) THEN
     INFO = (N / 2) * (N - 1)
  ELSE ! N odd
     INFO = N * ((N - 1) / 2)
  END IF

  W = WZERO
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,O,INFO) PRIVATE(H,K,P,Q) REDUCTION(MAX:W) IF(L .NE. 0)
  DO K = 1, INFO
     P = O(1,K)
     Q = O(2,K)
     IF (G(Q,P) .EQ. ZERO) THEN
        H = ABS(G(P,Q))
     ELSE IF (G(P,Q) .EQ. ZERO) THEN
        H = ABS(G(Q,P))
     ELSE ! none are 0
        H = CR_HYPOT(G(Q,P), G(P,Q))
     END IF
     IF ((H .GT. ZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
        CALL XENC(D(K), H, P, Q)
        W = MAX(W, D(K))
     ELSE ! no transformation
        D(K) = -H
     END IF
  END DO
  !$OMP END PARALLEL DO
  D(INFO+1) = W
