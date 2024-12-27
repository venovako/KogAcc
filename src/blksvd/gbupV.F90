  ! update V from the right
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,B,V,LDV,GB,VB,LDB,NB,O) PRIVATE(I,J) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO I = 1, NB
     IF (O(1,NB+I) .GT. 0) THEN
        J = 0
        CALL BROTC(M, B, V, LDV, O(1,I), O(2,I), VB(1,1,I), LDB, GB(B+1,1,I), LDB, J)
        IF (J .LT. 0) INFO = MIN(INFO, -10 * I - 9)
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .NE. 0) RETURN
