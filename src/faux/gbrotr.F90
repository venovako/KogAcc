  J = 0
  I = 2 * B
  IF (LDW .LT. B) J = -10
  IF (LDB .LT. I) J = -8
  IF (Q .LE. P) J = -6
  IF (P .LE. 0) J = -5
  IF (LDG .LT. N) J = -4
  IF (B .LE. 0) J = -2
  IF (N .LT. 2) J = -1
  IF (J .NE. 0) THEN
     INFO = J
     RETURN
  END IF
  I = N / B
  IF (Q .GT. I) J = -6
  IF (MOD(N, B) .NE. 0) J = -2
  IF (J .NE. 0) THEN
     INFO = J
     RETURN
  END IF

  DO J = 1, I
     IF ((INFO .NE. 0) .AND. ((J .EQ. P) .OR. (J .EQ. Q))) CYCLE
     GI = (P - 1) * B + 1
     GJ = (J - 1) * B + 1
     RI = 1
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ZERO, W(1,1), LDW)
     GI = (Q - 1) * B + 1
     RI = 1
     RJ = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ONE,  W(1,1), LDW)
     GI = (P - 1) * B + 1
     RI = B + 1
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ZERO, W(B+1,1), LDW)
     GI = (Q - 1) * B + 1
     RI = B + 1
     RJ = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, R(RI,RJ), LDB, G(GI,GJ), LDG, ONE,  W(B+1,1), LDW)
     ! copy back to G from W
     CALL LACPY('A', B, B, W(B+1,1), LDW, G(GI,GJ), LDG)
     GI = (P - 1) * B + 1
     CALL LACPY('A', B, B, W(1,1), LDW, G(GI,GJ), LDG)
  END DO
