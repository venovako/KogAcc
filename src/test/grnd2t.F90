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
  DO I = 1, SSIZE
     WRITE (ERROR_UNIT,*) ISEED(I)
  END DO
  ALLOCATE(G(2,2,N))
  F = -HUGE(F(1))
#ifdef UPPER
  ALLOCATE(U(2,2,N,2))
  ALLOCATE(V(2,2,N,2))
  ALLOCATE(S(2,N,2))
  ALLOCATE(INFO(N,2))
  ALLOCATE(E(3,N,2))
  !$OMP PARALLEL DEFAULT(NONE) SHARED(G,U,V,S,INFO,E,N) PRIVATE(I,J,K,L,M,Q) REDUCTION(MAX:F)
#else
  ALLOCATE(U(2,2,N))
  ALLOCATE(V(2,2,N))
  ALLOCATE(S(2,N))
  ALLOCATE(INFO(N))
  ALLOCATE(E(3,N))
  !$OMP PARALLEL DEFAULT(NONE) SHARED(G,U,V,S,INFO,E,N) PRIVATE(I,J,K,L,M,T) REDUCTION(MAX:F)
#endif
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
  CALL RANDOM_NUMBER(G(:,:,I:J))
  DO K = I, J
#ifdef UPPER
     ! use G(2,1,K) to determine the signs of the other three elements of G(:,:,K)
     M = EXPONENT(G(2,1,K))
     IF (IAND(M, 1) .NE. 0) G(1,1,K) = -G(1,1,K)
     G(2,1,K) = ZERO
     IF (IAND(M, 4) .NE. 0) G(1,2,K) = -G(1,2,K)
     IF (IAND(M, 8) .NE. 0) G(2,2,K) = -G(2,2,K)
     INFO(K,1) = 0
     CALL KSVD2(G(1,1,K), U(1,1,K,1), V(1,1,K,1), S(1,K,1), INFO(K,1))
     CALL KERR2(G(1,1,K), U(1,1,K,1), V(1,1,K,1), S(1,K,1), E(1,K,1), INFO(K,1))
     IF (INFO(K,1) .NE. 0) CALL STHALT('KSVD2')
     INFO(K,2) = 0
     CALL LWSV2(G(1,1,K), U(1,1,K,2), V(1,1,K,2), S(1,K,2), INFO(K,2))
     CALL KERR2(G(1,1,K), U(1,1,K,2), V(1,1,K,2), S(1,K,2), E(1,K,2), INFO(K,2))
     IF (INFO(K,2) .NE. 0) CALL STHALT('LWSV2')
     F(1) = MAX(F(1), E(1,K,1))
     F(2) = MAX(F(2), E(2,K,1))
     F(3) = MAX(F(3), E(3,K,1))
     F(4) = MAX(F(4), E(1,K,2))
     F(5) = MAX(F(5), E(2,K,2))
     F(6) = MAX(F(6), E(3,K,2))
     Q = E(1,K,2) / E(1,K,1)
     F(7) = MAX(F(7), Q)
     F(10) = MAX(F(10), -Q)
     Q = E(2,K,2) / E(2,K,1)
     F(8) = MAX(F(8), Q)
     F(11) = MAX(F(11), -Q)
     Q = E(3,K,2) / E(3,K,1)
     F(9) = MAX(F(9), Q)
     F(12) = MAX(F(12), -Q)
#else
     ! for each matrix, harvest an additional random number to determine the signs of the elements
     CALL RANDOM_NUMBER(T)
     M = EXPONENT(T)
     IF (IAND(M, 1) .NE. 0) G(1,1,K) = -G(1,1,K)
     IF (IAND(M, 2) .NE. 0) G(2,1,K) = -G(2,1,K)
     IF (IAND(M, 4) .NE. 0) G(1,2,K) = -G(1,2,K)
     IF (IAND(M, 8) .NE. 0) G(2,2,K) = -G(2,2,K)
     INFO(K) = 0
     CALL KSVD2(G(1,1,K), U(1,1,K), V(1,1,K), S(1,K), INFO(K))
     CALL KERR2(G(1,1,K), U(1,1,K), V(1,1,K), S(1,K), E(1,K), INFO(K))
     IF (INFO(K) .NE. 0) CALL STHALT('KSVD2')
     F(1) = MAX(F(1), E(1,K))
     F(2) = MAX(F(2), E(2,K))
     F(3) = MAX(F(3), E(3,K))
     F(4) = MAX(F(4), -E(1,K))
     F(5) = MAX(F(5), -E(2,K))
     F(6) = MAX(F(6), -E(3,K))
#endif
  END DO
  !$OMP END PARALLEL
#ifdef UPPER
  DO I = 1, 9
     WRITE (*,1,ADVANCE='NO') ',', F(I)
  END DO
  DO I = 10, 11
     WRITE (*,1,ADVANCE='NO') ',', -F(I)
  END DO
  WRITE (*,1) -F(12)
#else
  DO I = 1, 3
     WRITE (*,1,ADVANCE='NO') ',', F(I)
  END DO
  DO I = 4, 5
     WRITE (*,1,ADVANCE='NO') ',', -F(I)
  END DO
  WRITE (*,1) -F(6)
#endif
  DEALLOCATE(E)
  DEALLOCATE(INFO)
  DEALLOCATE(S)
  DEALLOCATE(V)
  DEALLOCATE(U)
  DEALLOCATE(G)
  DEALLOCATE(ISEED)
1 FORMAT(A,ES16.9E2)
! FORMAT(A,ES45.36E4)
