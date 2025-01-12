  ! update G from the left
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,B,G,LDG,GB,UB,LDB,NB,O) PRIVATE(I,J) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO I = 1, NB
     IF (O(1,NB+I) .GT. 0) THEN
        J = 0
        CALL CONJT(2*B, UB(1,1,I), LDB, J)
        IF (J .LT. 0) THEN
           INFO = MIN(INFO, -10 * I - 6)
        ELSE ! OK
           J = 1
           CALL BROTR(M, B, G, LDG, O(1,I), O(2,I), UB(1,1,I), LDB, GB(1,1,I), LDB, J)
           IF (J .LT. 0) INFO = MIN(INFO, -10 * I - 7)
        END IF
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .NE. 0) RETURN
