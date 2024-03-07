  CALL RANDOM_SEED(SIZE=SSIZE)
  IF (SSIZE .LE. 0) ERROR STOP 'seed size non-positive'
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 1) THEN
     IF (SSIZE .GT. 1) THEN
        WRITE (CLA,'(I1)') SSIZE
        CLA = 'args: N [SEED1 ... SEED'//TRIM(CLA)//']'
     ELSE ! SSIZE = 1
        CLA = 'args: N [SEED1]'
     END IF
     WRITE (ERROR_UNIT,*) TRIM(CLA)
     ERROR STOP 'All SEED arguments have to be given, or none of them.'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) ERROR STOP 'N <= 0'
  IF (I .EQ. 1) THEN
     ALLOCATE(ISEED(SSIZE))
     CALL RANDOM_SEED
     CALL RANDOM_SEED(GET=ISEED)
  ELSE IF (I .EQ. (SSIZE + 1)) THEN
     ALLOCATE(ISEED(SSIZE))
     DO I = 1, SSIZE
        CALL GET_COMMAND_ARGUMENT(I + 1, CLA)
        READ (CLA,*) ISEED(I)
     END DO
     CALL RANDOM_SEED(PUT=ISEED)
  ELSE ! a wrong SEED
     ERROR STOP 'invalid number of SEED arguments'
  END IF
#ifndef _OPENMP
  DO I = 1, SSIZE
     WRITE (ERROR_UNIT,*) ISEED(I)
  END DO
#endif
  Q = QZERO
  Q = -QONE / Q
  F = Q
  !$OMP PARALLEL DEFAULT(NONE) SHARED(N) PRIVATE(G,U,V,S,QG,QU,QV,QS,E,INFO,I,J,K,L,M,Q) REDUCTION(MAX:F)
  K = 0
  !$ K = INT(OMP_GET_THREAD_NUM())
  L = 1
  !$ L = INT(OMP_GET_NUM_THREADS())
  M = MOD(N, L)
  L = N / L
  IF (K .LT. M) THEN
     L = L + 1
     I = K * L + 1
  ELSE ! K >= M
     I = M * (L + 1) + (K - M) * L + 1
  END IF
  J = MIN(I + (L - 1), N)
  DO K = I, J
     CALL RANDOM_NUMBER(S)
     G(1,1) = CMPLX(S(1,1), S(2,1), D)
     G(2,1) = CMPLX(S(1,2), S(2,2), D)
     G(1,2) = CMPLX(S(1,3), S(2,3), D)
     G(2,2) = CMPLX(S(1,4), S(2,4), D)
     ! for each matrix, two additional random numbers are harvested to determine the signs of the elements
     M = EXPONENT(S(1,5))
     L = EXPONENT(S(2,5))
     IF (IAND(M, 1) .NE. 0) THEN
        IF (IAND(L, 1) .NE. 0) THEN
           G(1,1) = -G(1,1)
        ELSE ! no change of the imaginary part
           G(1,1) = CMPLX(-REAL(G(1,1)), AIMAG(G(1,1)), D)
        END IF
     ELSE IF (IAND(L, 1) .NE. 0) THEN
        G(1,1) = CMPLX(REAL(G(1,1)), -AIMAG(G(1,1)), D)
     END IF
     IF (IAND(M, 2) .NE. 0) THEN
        IF (IAND(L, 2) .NE. 0) THEN
           G(2,1) = -G(2,1)
        ELSE ! no change of the imaginary part
           G(2,1) = CMPLX(-REAL(G(2,1)), AIMAG(G(2,1)), D)
        END IF
     ELSE IF (IAND(L, 2) .NE. 0) THEN
        G(2,1) = CMPLX(REAL(G(2,1)), -AIMAG(G(2,1)), D)
     END IF
     IF (IAND(M, 4) .NE. 0) THEN
        IF (IAND(L, 4) .NE. 0) THEN
           G(1,2) = -G(1,2)
        ELSE ! no change of the imaginary part
           G(1,2) = CMPLX(-REAL(G(1,2)), AIMAG(G(1,2)), D)
        END IF
     ELSE IF (IAND(L, 4) .NE. 0) THEN
        G(1,2) = CMPLX(REAL(G(1,2)), -AIMAG(G(1,2)), D)
     END IF
     IF (IAND(M, 8) .NE. 0) THEN
        IF (IAND(L, 8) .NE. 0) THEN
           G(2,2) = -G(2,2)
        ELSE ! no change of the imaginary part
           G(2,2) = CMPLX(-REAL(G(2,2)), AIMAG(G(2,2)), D)
        END IF
     ELSE IF (IAND(L, 8) .NE. 0) THEN
        G(2,2) = CMPLX(REAL(G(2,2)), -AIMAG(G(2,2)), D)
     END IF
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
     L = -INFO
     CALL KERR2(G, U, V, S, E, INFO)
     Q = S(1,1)
     IF (L .NE. 0) Q = SCALE(Q, L)
     E(4) = MAX(ABS(QS(1) - Q) / QS(1), QZERO)
     Q = S(2,1)
     IF (L .NE. 0) Q = SCALE(Q, L)
     E(5) = MAX(ABS(QS(2) - Q) / QS(2), QZERO)
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
  !$OMP END PARALLEL
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
  DEALLOCATE(ISEED)
