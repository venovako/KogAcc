  I = INFO
  INFO = 0
  IF (LW .LT. 0) INFO = -10
  IF (LDB .LT. 0) INFO = -7
  IF (NB .LT. 0) INFO = -3
  IF (B2 .LT. 2) INFO = -2
  IF ((JS .LT. 0) .OR. (JS .GT. 7)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (NB .EQ. 0) RETURN
  J = 0
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(JS,B2,NB,GB,UB,VB,LDB,SB,WB,OD,OB,O) PRIVATE(I,L) REDUCTION(MIN:INFO,J) IF(I .NE. 0)
  DO I = 1, NB
     O(1,I) = -HUGE(0)
     O(1,I) = O(1,I) - 1
#ifdef ANIMATE
     DO L = 1, B2
        SB(L,I) = 0.0_K
     END DO
#endif
     L = JS + JOB
     CALL KSVD0(L, B2, GB(1,1,I), LDB, UB(1,1,I), LDB, VB(1,1,I), LDB, SB(1,I), WB(1,I), OB, OD(1,1,I), O(1,I))
     IF ((JS .EQ. 4) .OR. (JS .EQ. 7)) THEN
        L = B2
     ELSE ! not modified modulus
        L = B2 - 1
     END IF
     L = O(1,I) / L
     J = MIN(J, -L)
     O(2,I) = INT(WB(4,I)) ! GS
     IF (O(1,I) .LT. 0) THEN
        INFO = MIN(INFO, -100 * I + O(1,I))
#ifndef NDEBUG
     ELSE IF (.NOT. (WB(1,I) .LE. HUGE(WB(1,I)))) THEN
        INFO = MIN(INFO, -100 * I - 99)
     ELSE IF (.NOT. (WB(2,I) .LE. HUGE(WB(2,I)))) THEN
        INFO = MIN(INFO, -100 * I - 98)
     ELSE IF (.NOT. (WB(3,I) .LE. HUGE(WB(3,I)))) THEN
        INFO = MIN(INFO, -100 * I - 97)
#endif
     ELSE ! OK
        INFO = MIN(INFO, 0)
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (INFO .LT. 0) RETURN
  INFO = -J ! max # of sweeps
