#if (defined(MKL_DIRECT_CALL_SEQ) || defined(MKL_DIRECT_CALL))
#include "mkl_direct_call.fi"
#endif
#ifndef NDEBUG
  I = 0
  J = 2 * B
  IF (LDW .LT. B) I = -10
  IF (LDB .LT. J) I = -8
  IF (Q .LE. P) I = -6
  IF (P .LE. 0) I = -5
  IF (LDG .LT. M) I = -4
  IF (B .LE. 0) I = -2
  IF (M .LT. 2) I = -1
  IF (I .NE. 0) THEN
     INFO = I
     RETURN
  END IF
#endif
  J = M / B
#ifndef NDEBUG
  IF (Q .GT. J) I = -6
  IF (MOD(M, B) .NE. 0) I = -2
  IF (I .NE. 0) THEN
     INFO = I
     RETURN
  END IF
#endif
  DO I = 1, J
     IF ((INFO .NE. 0) .AND. ((I .EQ. P) .OR. (I .EQ. Q))) CYCLE
     GI = (I - 1) * B + 1
     GJ = (P - 1) * B + 1
     RI = 1
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ZERO, W(1,1), LDW)
     GJ = (Q - 1) * B + 1
     RI = B + 1
     RJ = 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ONE,  W(1,1), LDW)
     GJ = (P - 1) * B + 1
     RI = 1
     RJ = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ZERO, W(1,B+1), LDW)
     GJ = (Q - 1) * B + 1
     RI = B + 1
     RJ = B + 1
     CALL GEMM('N', 'N', B, B, B, ONE, G(GI,GJ), LDG, R(RI,RJ), LDB, ONE,  W(1,B+1), LDW)
     ! copy back to G from W
     CALL LACPY('A', B, B, W(1,B+1), LDW, G(GI,GJ), LDG)
     GJ = (P - 1) * B + 1
     CALL LACPY('A', B, B, W(1,1), LDW, G(GI,GJ), LDG)
  END DO
