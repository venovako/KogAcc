  IF (INFO .LT. 0) THEN
     L = -(INFO + 1)
     K = -1
  ELSE ! INFO .GE. 0
     L = INFO
     K = 0
  END IF
  INFO = 0

  IF (B .LT. 1) INFO = -4
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (MOD(N, B) .NE. 0) THEN
     INFO = -4
     RETURN
  END IF
  M = N / B
  IF (M .EQ. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,LDG,B,W,K,M) PRIVATE(I,J,P,Q) COLLAPSE(2) IF(L .NE. 0)
  DO Q = 1, M
     DO P = 1, M
        I = (P - 1) * B + 1
        J = (Q - 1) * B + 1
        IF (I .EQ. J) THEN
           W(P,Q) = NRM2O(B, B, G(I,J), LDG, K)
        ELSE ! off-diag block
           W(P,Q) = NRM2O(B, B, G(I,J), LDG, 0)
        END IF
     END DO
  END DO
  !$OMP END PARALLEL DO
