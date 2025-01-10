  ! update U from the right
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,B,U,LDU,GB,UB,LDB,NB,O) PRIVATE(I,J) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO I = 1, NB
     IF (O(1,NB+I) .GT. 0) THEN
        J = 0
        CALL BROTC(M, B, U, LDU, O(1,I), O(2,I), UB(1,1,I), LDB, GB(1,1,I), LDB, J)
        IF (J .LT. 0) THEN
           INFO = MIN(INFO, -10 * I - 5)
        ELSE ! OK
           INFO = MIN(INFO, 0)
        END IF
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .NE. 0) RETURN
