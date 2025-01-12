  I = INFO
  INFO = 0
#ifndef NDEBUG
  IF (LD .LT. 0) INFO = -12
  IF (LW .LT. 0) INFO = -10
  IF (LDB .LT. 0) INFO = -7
  IF (NB .LT. 0) INFO = -3
  IF (B2 .LT. 2) INFO = -2
  IF ((JS .NE. 3) .AND. (JS .NE. 6)) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (NB .EQ. 0) RETURN
  J = 0
  IF (I .GE. 0) THEN
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(JS,B2,NB,GB,UB,VB,LDB,SB,WB,DB,OD,OB,O) PRIVATE(I) REDUCTION(MIN:INFO,J) IF(I .NE. 0)
     DO I = 1, NB
        O(1,I) = HUGE(0)
        O(2,I) = JOB + JS
        CALL KSVDD(O(2,I), B2, GB(1,1,I),LDB, UB(1,1,I),LDB, VB(1,1,I),LDB, SB(1,I), WB(1,I), DB(1,I), OB, OD(1,1,I), O(1,I))
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
#ifndef NDEBUG
        ELSE ! OK
           INFO = MIN(INFO, 0)
#endif
        END IF
     END DO
!$OMP END PARALLEL DO
  ELSE ! I .LT. 0
     I = I + 1
!$OMP PARALLEL DO DEFAULT(NONE) SHARED(JS,B2,NB,GB,UB,VB,LDB,SB,WB,DB,OD,OB,O) PRIVATE(I) REDUCTION(MIN:INFO,J) IF(I .NE. 0)
     DO I = 1, NB
        O(1,I) = HUGE(0)
        O(2,I) = JOB + JS
        CALL KKSVDD(O(2,I), B2, GB(1,1,I),LDB, UB(1,1,I),LDB, VB(1,1,I),LDB, SB(1,I), WB(1,I), DB(1,I), OB, OD(1,1,I), O(1,I))
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
#ifndef NDEBUG
        ELSE ! OK
           INFO = MIN(INFO, 0)
#endif
        END IF
     END DO
!$OMP END PARALLEL DO
  END IF
  IF (INFO .LT. 0) RETURN
  INFO = -J ! max # of steps
