  I = INFO
  INFO = 0
#ifndef NDEBUG
  IF (LW .LT. 0) INFO = -10
  IF (LDB .LT. 0) INFO = -7
  IF (NB .LT. 0) INFO = -3
  IF (B2 .LT. 2) INFO = -2
  IF (INFO .NE. 0) RETURN
#endif
  IF (NB .EQ. 0) RETURN
  J = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(JS,B2,NB,GB,UB,VB,LDB,SB,WB,OD,OB,O) PRIVATE(I,L) REDUCTION(MIN:INFO,J) IF(I .NE. 0)
  DO I = 1, NB
#ifdef ANIMATE
     DO L = 1, B2
        SB(L,I) = 0.0_K
     END DO
#endif
     L = HUGE(0)
     IF (JS .GE. 0) THEN
        O(2,I) = JOB + JS
     ELSE ! JS .LT. 0
        L = -L - 1
        O(2,I) = JOB - (JS + 1)
     END IF
     O(1,I) = L
     CALL KSVD0(O(2,I), B2, GB(1,1,I), LDB, UB(1,1,I), LDB, VB(1,1,I), LDB, SB(1,I), WB(1,I), OB, OD(1,1,I), O(1,I))
     J = MIN(J, -O(1,I))
     O(2,I) = INT(WB(4,I)) ! GS
     IF (O(1,I) .LT. 0) THEN
        INFO = MIN(INFO, -100 * I + O(1,I))
     ELSE IF (.NOT. (WB(1,I) .LE. HUGE(WB(1,I)))) THEN
        INFO = MIN(INFO, -100 * I - 99)
     ELSE IF (.NOT. (WB(2,I) .LE. HUGE(WB(2,I)))) THEN
        INFO = MIN(INFO, -100 * I - 98)
     ELSE IF (.NOT. (WB(3,I) .LE. HUGE(WB(3,I)))) THEN
        INFO = MIN(INFO, -100 * I - 97)
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .LT. 0) RETURN
  INFO = -J ! max # of sweeps
