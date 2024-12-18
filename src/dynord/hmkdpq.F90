  M = (N * (N - 1)) / 2
  ! build D and determine its largest element
  W = MONE
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,M,O) PRIVATE(H,K,P,Q) REDUCTION(MAX:W) IF(L .NE. 0)
  DO K = 1, M
     P = O(1,K)
     Q = O(2,K)
     H = CR_HYPOT(CR_HYPOT(REAL(G(Q,P)), AIMAG(G(Q,P))), CR_HYPOT(REAL(G(P,Q)), AIMAG(G(P,Q))))
     IF ((H .GT. ZERO) .OR. (AIMAG(G(P,P)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(P,P))) .NE. ONE) .OR. &
          (AIMAG(G(Q,Q)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(Q,Q))) .NE. ONE) .OR. &
          (REAL(G(P,P)) .LT. REAL(G(Q,Q)))) THEN
        CALL XENC(D(K), H, P, Q)
        W = MAX(W, D(K))
     ELSE ! no transformation
        D(K) = MONE
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (W .LE. WZERO) RETURN

  ! find the remaining pivots
  DO INFO = 1, N/2
     P = 0; Q = 0
     CALL XDEC(W, P, Q)
     K = M + INFO
     O(1,K) = P
     O(2,K) = Q
     IF (INFO .GE. (N / 2)) EXIT
     W = MONE
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W) IF(L .NE. 0)
     DO K = 1, M
        IF (D(K) .GT. WZERO) THEN
           I = 0; J = 0
           CALL XDEC(D(K), I, J)
           IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
              W = MAX(W, D(K))
           ELSE ! colliding
              D(K) = MONE
           END IF
        END IF
     END DO
     !$OMP END PARALLEL DO
     IF (W .LE. WZERO) EXIT
  END DO
