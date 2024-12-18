  L = INFO
  INFO = 0
  M = 2 * B
  IF (L .LT. 0) INFO = -9
  IF (NB .LT. 0) INFO = -7
  IF (LDB .LT. M) INFO = -6
  IF (B .LT. 2) INFO = -4
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. (NB * M)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (NB .EQ. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,G,B,GB,LDB,NB,O) PRIVATE(I,J,K,L,M,P,Q) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO K = 1, NB
     P = O(1,K)
     Q = O(2,K)
     L = (P - 1) * B
     M = (Q - 1) * B
     IF ((M .LT. 0) .OR. (M .GE. N) .OR. (L .LT. 0) .OR. (L .GE. N) .OR. (M .LE. L)) THEN
        INFO = MIN(INFO, K)
     ELSE ! all OK
        INFO = MIN(INFO, 0)
        L = (P - 1) * B
        M = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,M+J) = GB(I,J,K)
           END DO
        END DO
        L = (Q - 1) * B
        M = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,M+J) = GB(B+I,J,K)
           END DO
        END DO
        L = (P - 1) * B
        M = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,M+J) = GB(I,B+J,K)
           END DO
        END DO
        L = (Q - 1) * B
        M = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              G(L+I,M+J) = GB(B+I,B+J,K)
           END DO
        END DO
     END IF
  END DO
  !$OMP END PARALLEL DO
