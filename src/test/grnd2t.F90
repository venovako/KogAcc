  CALL RANDOM_SEED(SIZE=SSIZE)
  IF (SSIZE .LE. 0) STOP 'seed size non-positive'
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 1) THEN
     IF (SSIZE .GT. 1) THEN
        WRITE (CLA,'(I1)') SSIZE
        CLA = 'args: N [SEED1 ... SEED'//TRIM(CLA)//']'
     ELSE ! SSIZE = 1
        CLA = 'args: N [SEED1]'
     END IF
     WRITE (ERROR_UNIT,*) TRIM(CLA)
     STOP 'All SEED arguments have to be given, or none of them.'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  READ (CLA,*) N
  IF (N .LE. 0) STOP 'N <= 0'
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
     STOP 'invalid number of SEED arguments'
  END IF
#ifndef _OPENMP
  DO I = 1, SSIZE
     WRITE (ERROR_UNIT,*) ISEED(I)
  END DO
#endif
  L = 1
  !$ L = MAX(L, INT(OMP_GET_MAX_THREADS()))
#ifdef UPPER
  ALLOCATE(NSTIME(2,L))
#else
  ALLOCATE(NSTIME(1,L))
#endif
#ifdef _WIN32
  Q9 = PVN_TIME_MONO_FREQ()
#else
  Q9 = 1.0E9_REAL128
#endif
  !DIR$ VECTOR ALWAYS
  NSTIME = 0_c_long_long
  Q = QZERO
  Q = -QONE / Q
  F = Q
#ifdef UPPER
  !$OMP PARALLEL DEFAULT(NONE) SHARED(N,NSTIME,Q9) PRIVATE(G,U,V,S,QG,QU,QV,QS,E,INFO,I,J,K,L,M,P,Q,NS) REDUCTION(MAX:F)
#else
  !$OMP PARALLEL DEFAULT(NONE) SHARED(N,NSTIME,Q9) PRIVATE(G,U,V,S,QG,QU,QV,QS,E,INFO,I,J,K,L,M,P,Q,T,NS) REDUCTION(MAX:F)
#endif
  K = 0
  !$ K = INT(OMP_GET_THREAD_NUM())
  P = K + 1
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
     CALL RANDOM_NUMBER(G)
#ifdef UPPER
#include "grnd2u.F90"
#else
#include "grnd2g.F90"
#endif
  END DO
  !$OMP END PARALLEL
#ifdef UPPER
  L = 18
#else
  L = 8
#endif
  NS(1) = SUM(NSTIME(1,:))
  F(1,L) = NS(1) / (N * Q9)
#ifdef UPPER
  NS(2) = SUM(NSTIME(2,:))
  F(2,L) = NS(2) / (N * Q9)
#else
  F(2,L) = QZERO
#endif
  WRITE (OUTPUT_UNIT,'(I11)',ADVANCE='NO') N
  DO K = 1, L-1
     WRITE (OUTPUT_UNIT,1,ADVANCE='NO') ',', F(1,K)
  END DO
  WRITE (OUTPUT_UNIT,1) ',', F(1,L)
  FLUSH(OUTPUT_UNIT)
  WRITE (ERROR_UNIT,'(I11)',ADVANCE='NO') N
  DO K = 1, L-2
     WRITE (ERROR_UNIT,1,ADVANCE='NO') ',', -F(2,K)
  END DO
  WRITE (ERROR_UNIT,1,ADVANCE='NO') ',', F(2,L-1)
  WRITE (ERROR_UNIT,1) ',', F(2,L)
  FLUSH(ERROR_UNIT)
  DEALLOCATE(NSTIME)
  DEALLOCATE(ISEED)
