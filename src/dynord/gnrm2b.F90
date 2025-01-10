  IF (INFO .LT. 0) THEN
     L = -(INFO + 1)
     K = -1
  ELSE ! INFO .GE. 0
     L = INFO
     K = 0
  END IF
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
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,LDG,B,W,K,N) PRIVATE(I,J,P,Q) COLLAPSE(2) IF(L .NE. 0)
  DO Q = 1, N
     DO P = 1, N
        I = (P - 1) * B + 1
        J = (Q - 1) * B + 1
        IF (P .EQ. Q) THEN
           W(P,Q) = NRM2O(B, B, G(I,J), LDG, K)
        ELSE ! off-diag block
           W(P,Q) = NRM2O(B, B, G(I,J), LDG, 0)
        END IF
     END DO
  END DO
  !$OMP END PARALLEL DO
