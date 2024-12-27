  L = INFO
  INFO = 0
  N = 2 * B
  IF (L .LT. 0) INFO = -9
  IF (NB .LT. 0) INFO = -7
  IF (LDB .LT. N) INFO = -6
  IF (B .LT. 2) INFO = -4
  IF (LDG .LT. M) INFO = -3
  IF (M .LT. (NB * N)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (NB .EQ. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,G,B,GB,LDB,NB,O) PRIVATE(I,J,K,L,N,P,Q) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO K = 1, NB
     P = O(1,K)
     Q = O(2,K)
     L = (P - 1) * B
     N = (Q - 1) * B
     IF ((N .LT. 0) .OR. (N .GE. M) .OR. (L .LT. 0) .OR. (L .GE. M) .OR. (N .LE. L)) THEN
        INFO = MIN(INFO, -10 - K)
     ELSE ! all OK
        INFO = MIN(INFO, 0)
        L = (P - 1) * B
        N = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,N+J) = GB(I,J,K)
           END DO
        END DO
        L = (Q - 1) * B
        N = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,N+J) = GB(B+I,J,K)
           END DO
        END DO
        L = (P - 1) * B
        N = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,N+J) = GB(I,B+J,K)
           END DO
        END DO
        L = (Q - 1) * B
        N = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,N+J) = GB(B+I,B+J,K)
           END DO
        END DO
     END IF
  END DO
  !$OMP END PARALLEL DO
