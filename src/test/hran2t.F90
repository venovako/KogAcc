  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 1) STOP 'args: N'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N <= 0'
  Q = QZERO
  Q = -QONE / Q
  F = Q
  K = 0
  L = 1
  M = MOD(N, L)
  L = N / L
  IF (K .LT. M) THEN
     L = L + 1
     I = K * L + 1
  ELSE ! K >= M
     I = M * (L + 1) + (K - M) * L + 1
  END IF
  J = MIN(I + (L - 1), N)
  OPEN(NEWUNIT=M,FILE='/dev/random',ACCESS='STREAM',ACTION='READ',STATUS='OLD')
  DO K = I, J
     G(1,1) = CMPLX(RSAFE(M), RSAFE(M), D)
     G(2,1) = CMPLX(RSAFE(M), RSAFE(M), D)
     G(1,2) = CMPLX(RSAFE(M), RSAFE(M), D)
     G(2,2) = CMPLX(RSAFE(M), RSAFE(M), D)
     INFO = 0
     QG(1,1) = G(1,1)
     QG(2,1) = G(2,1)
     QG(1,2) = G(1,2)
     QG(2,2) = G(2,2)
     CALL YKSVD2(QG, QU, QV, QS, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('YKSVD2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        QS(1) = SCALE(QS(1), L)
        QS(2) = SCALE(QS(2), L)
        INFO = 0
     END IF
     Q = QS(1) / QS(2)
     IF (Q .NE. Q) THEN
        Q = QZERO
        Q = QONE / Q
     END IF
     ! \kappa_2(G)
     F(1,1) = MAX(Q, F(1,1))
     F(2,1) = MAX(-Q, F(2,1))
     CALL KSVD2(G, U, V, S, INFO)
     IF (INFO .LE. -HUGE(INFO)) CALL STHALT('KSVD2')
     IF (INFO .NE. 0) THEN
        L = -INFO
        S(1) = SCALE(S(1), L)
        S(2) = SCALE(S(2), L)
        INFO = 0
     END IF
     CALL KERR2(G, U, V, S, E, INFO)
     E(4) = MAX(ABS(QS(1) - S(1)) / QS(1), QZERO)
     E(5) = MAX(ABS(QS(2) - S(2)) / QS(2), QZERO)
     F(1,2) = MAX(E(1), F(1,2))
     F(2,2) = MAX(-E(1), F(2,2))
     F(1,3) = MAX(E(2), F(1,3))
     F(2,3) = MAX(-E(2), F(2,3))
     F(1,4) = MAX(E(3), F(1,4))
     F(2,4) = MAX(-E(3), F(2,4))
     F(1,5) = MAX(E(4), F(1,5))
     F(2,5) = MAX(-E(4), F(2,5))
     F(1,6) = MAX(E(5), F(1,6))
     F(2,6) = MAX(-E(5), F(2,6))
  END DO
  L = 6
  WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') N
  DO K = 1, L-1
     WRITE (OUTPUT_UNIT,1,ADVANCE='NO') ',', F(1,K)
  END DO
  WRITE (OUTPUT_UNIT,1) ',', F(1,L)
  WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') N
  DO K = 1, L-1
     WRITE (ERROR_UNIT,1,ADVANCE='NO') ',', -F(2,K)
  END DO
  WRITE (ERROR_UNIT,1) ',', -F(2,L)
  CLOSE(M)
