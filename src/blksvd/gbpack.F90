  L = INFO
  INFO = 0
#ifndef NDEBUG
  N = 2 * B
  IF (L .LT. 0) INFO = -9
  IF (NB .LT. 0) INFO = -7
  IF (LDB .LT. N) INFO = -6
  IF (B .LT. 1) INFO = -4
  IF (LDG .LT. M) INFO = -3
  IF (M .LT. (NB * N)) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (NB .EQ. 0) RETURN

#ifdef NDEBUG
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,G,B,GB,LDB,NB,O) PRIVATE(I,J,K,L,N,P,Q) IF(L .NE. 0)
#else
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,G,B,GB,LDB,NB,O) PRIVATE(I,J,K,L,N,P,Q) REDUCTION(MIN:INFO) IF(L .NE. 0)
#endif
  DO K = 1, NB
     P = O(1,K)
     Q = O(2,K)
#ifndef NDEBUG
     IF ((P .LE. 0) .OR. (Q .LE. 0) .OR. (P .GE. Q)) THEN
        INFO = MIN(INFO, -10 - K)
     ELSE ! OK
#endif
        L = (P - 1) * B
        N = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(I,J,K) = G(L+I,N+J)
           END DO
        END DO
        L = (Q - 1) * B
        N = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(B+I,J,K) = G(L+I,N+J)
           END DO
#ifndef NDEBUG
           !DIR$ VECTOR ALWAYS
           DO I = 2*B+1, LDB
              GB(I,J,K) = ZERO
           END DO
#endif
        END DO
        L = (P - 1) * B
        N = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(I,B+J,K) = G(L+I,N+J)
           END DO
        END DO
        L = (Q - 1) * B
        N = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(B+I,B+J,K) = G(L+I,N+J)
           END DO
#ifndef NDEBUG
           !DIR$ VECTOR ALWAYS
           DO I = 2*B+1, LDB
              GB(I,B+J,K) = ZERO
           END DO
#endif
        END DO
#ifndef NDEBUG
     END IF
#endif
  END DO
  !$OMP END PARALLEL DO
