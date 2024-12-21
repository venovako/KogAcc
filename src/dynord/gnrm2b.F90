  IF (INFO .LT. 0) THEN
     L = -(INFO + 1)
     M = -1
  ELSE ! INFO .GE. 0
     L = INFO
     M = 0
  END IF
  INFO = 0

  IF (B .LT. 1) INFO = -4
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  K = MOD(N, B)
  IF (K .NE. 0) THEN
     INFO = -4
     RETURN
  END IF
  N_B = N / B
  IF (N_B .EQ. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,LDG,B,W,N_B,M) PRIVATE(I,J,K,P,Q) COLLAPSE(2) IF(L .NE. 0)
  DO Q = 1, N_B
     DO P = 1, N_B
        I = (P - 1) * B + 1
        J = (Q - 1) * B + 1
        K = (Q - 1) * N_B + P
        IF (I .EQ. J) THEN
           W(K) = NRM2O(B, B, G(I,J), LDG, M)
        ELSE ! off-diag block
           W(K) = NRM2O(B, B, G(I,J), LDG, 0)
        END IF
     END DO
  END DO
  !$OMP END PARALLEL DO
