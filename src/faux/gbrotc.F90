  I = 0
  M = 2 * B
  IF (LDW .LT. B) I = -10
  IF (LDB .LT. M) I = -8
  IF (Q .LE. P) I = -6
  IF (P .LE. 0) I = -5
  IF (LDG .LT. N) I = -4
  IF (B .LE. 0) I = -2
  IF (N .LT. 2) I = -1
  IF (I .NE. 0) THEN
     INFO = I
     RETURN
  END IF
  L = N / B
  IF (Q .GT. L) I = -6
  IF (MOD(N, B) .NE. 0) I = -2
  IF (I .NE. 0) THEN
     INFO = I
     RETURN
  END IF

  DO I = 1, L
     IF ((INFO .NE. 0) .AND. ((I .EQ. P) .OR. (I .EQ. Q))) CYCLE
     GI = (I - 1) * B + 1
     GJ = (P - 1) * B + 1
     RI = 1
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ZERO, W, LDW)
     J = GJ
     GJ = (Q - 1) * B + 1
     RI = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ONE,  W, LDW)
     CALL LACPY('A', B, B, W, LDW, G(GI,J), LDG)
     M = GJ
     GJ = J
     J = M
     RJ = RI
     RI = 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ZERO, W, LDW)
     GJ = J
     RI = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ONE,  W, LDW)
     CALL LACPY('A', B, B, W, LDW, G(GI,J), LDG)
  END DO
