  ! update G from the right
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,B,G,LDG,GB,VB,LDB,NB,O) PRIVATE(I,J) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO I = 1, NB
     IF (O(1,NB+I) .GT. 0) THEN
        J = 1
        CALL BROTC(N, B, G, LDG, O(1,I), O(2,I), VB(1,1,I), LDB, GB(1,1,I), LDB, J)
        IF (J .LT. 0) INFO = MIN(INFO, -10 * I - 8)
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .NE. 0) RETURN
