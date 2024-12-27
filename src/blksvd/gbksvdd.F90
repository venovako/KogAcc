  I = INFO
  INFO = 0
  J = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(B2,NB,GB,UB,VB,LDB,SB,WB,DB,OD,OB,O) PRIVATE(I) REDUCTION(MIN:INFO,J) IF(I .NE. 0)
  DO I = 1, NB
     O(1,I) = -HUGE(0)
     O(1,I) = O(1,I) - 1
     CALL KSVDD(JOB, B2, GB(1,1,I), LDB, UB(1,1,I), LDB, VB(1,1,I), LDB, SB(1,I), WB(1,I), DB(1,I), OB, OD(1,1,I), O(1,I))
     J = MIN(J, -O(1,I))
     O(2,I) = INT(WB(4,I)) ! GS
     IF (O(1,I) .LT. 0) THEN
        INFO = MIN(INFO, -100 * I + O(1,I))
#ifndef NDEBUG
     ELSE IF (.NOT. (WB(1,I) .LE. HUGE(ZERO))) THEN
        INFO = MIN(INFO, -100 * I - 99)
     ELSE IF (.NOT. (WB(2,I) .LE. HUGE(ZERO))) THEN
        INFO = MIN(INFO, -100 * I - 98)
     ELSE IF (.NOT. (WB(3,I) .LE. HUGE(ZERO))) THEN
        INFO = MIN(INFO, -100 * I - 97)
     ELSE ! OK
        INFO = MIN(INFO, 0)
#endif
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .LT. 0) RETURN
  INFO = -J ! max # of steps
