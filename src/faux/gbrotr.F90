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

  DO J = 1, L
     IF ((INFO .NE. 0) .AND. ((J .EQ. P) .OR. (J .EQ. Q))) CYCLE
     GI = (P - 1) * B + 1
     GJ = (J - 1) * B + 1
     RI = 1
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ZERO, W(1,1), LDW)
     I = GI
     GI = (Q - 1) * B + 1
     RJ = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ONE,  W(1,1), LDW)
     M = GI
     GI = I
     I = M
     RI = RJ
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ZERO, W(B+1,1), LDW)
     M = GI
     GI = I
     I = M
     RJ = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ONE,  W(B+1,1), LDW)
     CALL LACPY('A', B, B, W(1,1), LDW, G(I,GJ), LDG)
     CALL LACPY('A', B, B, W(B+1,1), LDW, G(GI,GJ), LDG)
  END DO
