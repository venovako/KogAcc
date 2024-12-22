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
     H = CR_HYPOT(CR_HYPOT(REAL(G(Q,P)), AIMAG(G(Q,P))), CR_HYPOT(REAL(G(P,Q)), AIMAG(G(P,Q))))
     IF ((H .GT. ZERO) .OR. (AIMAG(G(P,P)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(P,P))) .NE. ONE) .OR. &
          (AIMAG(G(Q,Q)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(Q,Q))) .NE. ONE) .OR. &
          (REAL(G(P,P)) .LT. REAL(G(Q,Q)))) THEN
        CALL XENC(D(K), H, P, Q)
        W = MAX(W, D(K))
     ELSE ! no transformation
        D(K) = -H
     END IF
  END DO
  !$OMP END PARALLEL DO
  D(INFO+1) = W
