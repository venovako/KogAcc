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
  ALLOCATE(G(2,2,N))
  F = -HUGE(Q)
#ifdef UPPER
  ALLOCATE(U(2,2,N,3))
  ALLOCATE(V(2,2,N,3))
  ALLOCATE(S(2,N,3))
  ALLOCATE(INFO(N,3))
  ALLOCATE(E(3,N,3))
#else
  ALLOCATE(U(2,2,N,2))
  ALLOCATE(V(2,2,N,2))
  ALLOCATE(S(2,N,2))
  ALLOCATE(INFO(N,2))
  ALLOCATE(E(3,N,2))
#endif
  !$OMP PARALLEL DEFAULT(NONE) SHARED(G,U,V,S,INFO,E,N) PRIVATE(I,J,K,L,M,Q,T) REDUCTION(MAX:F)
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
#if (UPPER > 0)
     ! prevent the pivoted QR from taking place
     L = MAX(EXPONENT(G(1,2,K)), EXPONENT(G(2,2,K))) - EXPONENT(G(1,1,K))
     IF (L .GE. 0) THEN
        L = L + UPPER
     ELSE IF (-L .LT. UPPER) THEN
        L = UPPER + L
     ELSE ! L .GE. -UPPER
        L = 0
     END IF
     T = SCALE(G(1,1,K), L)
     IF (.NOT. (T .LE. HUGE(T))) CALL STHALT('UPPER')
     G(1,1,K) = T
#endif
     INFO(K,1) = 0
     CALL KSVD2(G(1,1,K), U(1,1,K,1), V(1,1,K,1), S(1,K,1), INFO(K,1))
     CALL KERR2(G(1,1,K), U(1,1,K,1), V(1,1,K,1), S(1,K,1), E(1,K,1), INFO(K,1))
     IF (INFO(K,1) .NE. 0) CALL STHALT('KSVD2')
     INFO(K,2) = 0
     CALL KALT2(G(1,1,K), U(1,1,K,2), V(1,1,K,2), S(1,K,2), INFO(K,2))
     CALL KERR2(G(1,1,K), U(1,1,K,2), V(1,1,K,2), S(1,K,2), E(1,K,2), INFO(K,2))
     IF (INFO(K,2) .NE. 0) CALL STHALT('KALT2')
     INFO(K,3) = 0
     CALL LWSV2(G(1,1,K), U(1,1,K,3), V(1,1,K,3), S(1,K,3), INFO(K,3))
     CALL KERR2(G(1,1,K), U(1,1,K,3), V(1,1,K,3), S(1,K,3), E(1,K,3), INFO(K,3))
     IF (INFO(K,3) .NE. 0) CALL STHALT('LWSV2')
     F(1) = MAX(F(1), E(1,K,1))
     F(2) = MAX(F(2), E(2,K,1))
     F(3) = MAX(F(3), E(3,K,1))
     F(4) = MAX(F(4), -E(1,K,1))
     F(5) = MAX(F(5), -E(2,K,1))
     F(6) = MAX(F(6), -E(3,K,1))
     F(7) = MAX(F(7), E(1,K,2))
     F(8) = MAX(F(8), E(2,K,2))
     F(9) = MAX(F(9), E(3,K,2))
     F(10) = MAX(F(10), -E(1,K,2))
     F(11) = MAX(F(11), -E(2,K,2))
     F(12) = MAX(F(12), -E(3,K,2))
     F(13) = MAX(F(13), E(1,K,3))
     F(14) = MAX(F(14), E(2,K,3))
     F(15) = MAX(F(15), E(3,K,3))
     F(16) = MAX(F(16), -E(1,K,3))
     F(17) = MAX(F(17), -E(2,K,3))
     F(18) = MAX(F(18), -E(3,K,3))
     Q = E(1,K,3) / E(1,K,1)
     F(19) = MAX(F(19), Q)
     F(22) = MAX(F(22), -Q)
     Q = E(2,K,3) / E(2,K,1)
     F(20) = MAX(F(20), Q)
     F(23) = MAX(F(23), -Q)
     Q = E(3,K,3) / E(3,K,1)
     F(21) = MAX(F(21), Q)
     F(24) = MAX(F(24), -Q)
     Q = E(1,K,3) / E(1,K,2)
     F(25) = MAX(F(25), Q)
     F(28) = MAX(F(28), -Q)
     Q = E(2,K,3) / E(2,K,2)
     F(26) = MAX(F(26), Q)
     F(29) = MAX(F(29), -Q)
     Q = E(3,K,3) / E(3,K,2)
     F(27) = MAX(F(27), Q)
     F(30) = MAX(F(30), -Q)
#else
     ! for each matrix, harvest an additional random number to determine the signs of the elements
     CALL RANDOM_NUMBER(T)
     M = EXPONENT(T)
     IF (IAND(M, 1) .NE. 0) G(1,1,K) = -G(1,1,K)
     IF (IAND(M, 2) .NE. 0) G(2,1,K) = -G(2,1,K)
     IF (IAND(M, 4) .NE. 0) G(1,2,K) = -G(1,2,K)
     IF (IAND(M, 8) .NE. 0) G(2,2,K) = -G(2,2,K)
     INFO(K,1) = 0
     CALL KSVD2(G(1,1,K), U(1,1,K,1), V(1,1,K,1), S(1,K,1), INFO(K,1))
     CALL KERR2(G(1,1,K), U(1,1,K,1), V(1,1,K,1), S(1,K,1), E(1,K,1), INFO(K,1))
     IF (INFO(K,1) .NE. 0) CALL STHALT('KSVD2')
     INFO(K,2) = 0
     CALL KALT2(G(1,1,K), U(1,1,K,2), V(1,1,K,2), S(1,K,2), INFO(K,2))
     CALL KERR2(G(1,1,K), U(1,1,K,2), V(1,1,K,2), S(1,K,2), E(1,K,2), INFO(K,2))
     IF (INFO(K,2) .NE. 0) CALL STHALT('KALT2')
     F(1) = MAX(F(1), E(1,K,1))
     F(2) = MAX(F(2), E(2,K,1))
     F(3) = MAX(F(3), E(3,K,1))
     F(4) = MAX(F(4), -E(1,K,1))
     F(5) = MAX(F(5), -E(2,K,1))
     F(6) = MAX(F(6), -E(3,K,1))
     F(7) = MAX(F(7), E(1,K,2))
     F(8) = MAX(F(8), E(2,K,2))
     F(9) = MAX(F(9), E(3,K,2))
     F(10) = MAX(F(10), -E(1,K,2))
     F(11) = MAX(F(11), -E(2,K,2))
     F(12) = MAX(F(12), -E(3,K,2))
     Q = E(1,K,2) / E(1,K,1)
     F(13) = MAX(F(13), Q)
     F(16) = MAX(F(16), -Q)
     Q = E(2,K,2) / E(2,K,1)
     F(14) = MAX(F(14), Q)
     F(17) = MAX(F(17), -Q)
     Q = E(3,K,2) / E(3,K,1)
     F(15) = MAX(F(15), Q)
     F(18) = MAX(F(18), -Q)
#endif
  END DO
  !$OMP END PARALLEL
  WRITE (*,'(I10)',ADVANCE='NO') N
  WRITE (*,1,ADVANCE='NO') ',', F(1)
  WRITE (*,1,ADVANCE='NO') ',', F(2)
  WRITE (*,1,ADVANCE='NO') ',', F(3)
  WRITE (*,1,ADVANCE='NO') ',', -F(4)
  WRITE (*,1,ADVANCE='NO') ',', -F(5)
  WRITE (*,1,ADVANCE='NO') ',', -F(6)
  WRITE (*,1,ADVANCE='NO') ',', F(7)
  WRITE (*,1,ADVANCE='NO') ',', F(8)
  WRITE (*,1,ADVANCE='NO') ',', F(9)
  WRITE (*,1,ADVANCE='NO') ',', -F(10)
  WRITE (*,1,ADVANCE='NO') ',', -F(11)
  WRITE (*,1,ADVANCE='NO') ',', -F(12)
  WRITE (*,1,ADVANCE='NO') ',', F(13)
  WRITE (*,1,ADVANCE='NO') ',', F(14)
  WRITE (*,1,ADVANCE='NO') ',', F(15)
  WRITE (*,1,ADVANCE='NO') ',', -F(16)
  WRITE (*,1,ADVANCE='NO') ',', -F(17)
#ifdef UPPER
  WRITE (*,1,ADVANCE='NO') ',', -F(18)
  WRITE (*,1,ADVANCE='NO') ',', F(19)
  WRITE (*,1,ADVANCE='NO') ',', F(20)
  WRITE (*,1,ADVANCE='NO') ',', F(21)
  WRITE (*,1,ADVANCE='NO') ',', -F(22)
  WRITE (*,1,ADVANCE='NO') ',', -F(23)
  WRITE (*,1,ADVANCE='NO') ',', -F(24)
  WRITE (*,1,ADVANCE='NO') ',', F(25)
  WRITE (*,1,ADVANCE='NO') ',', F(26)
  WRITE (*,1,ADVANCE='NO') ',', F(27)
  WRITE (*,1,ADVANCE='NO') ',', -F(28)
  WRITE (*,1,ADVANCE='NO') ',', -F(29)
  WRITE (*,1) ',', -F(30)
#else
  WRITE (*,1) ',', -F(18)
#endif
  DEALLOCATE(E)
  DEALLOCATE(INFO)
  DEALLOCATE(S)
  DEALLOCATE(V)
  DEALLOCATE(U)
  DEALLOCATE(G)
  DEALLOCATE(ISEED)
