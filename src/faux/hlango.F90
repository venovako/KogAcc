  S = ZERO
  I = INFO
  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (I .GE. 0) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC,SM) SHARED(G,N) REDUCTION(MAX:S) IF(I .NE. 0)
     DO J = 1, N
        DO I = 1, N
           SC = ABS(REAL(G(I,J)))
           SM = ABS(AIMAG(G(I,J)))
           S = MAX(S, SC, SM)
        END DO
     END DO
     !$OMP END PARALLEL DO
  ELSE ! I .LT. 0
     I = I + 1
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC,SM) SHARED(G,N) REDUCTION(MAX:S,INFO) IF(I .NE. 0)
     DO J = 1, N
        DO I = 1, N
           SC = ABS(REAL(G(I,J)))
           SM = ABS(AIMAG(G(I,J)))
           IF ((.NOT. (SC .LE. HUGE(SC))) .OR. (.NOT. (SM .LE. HUGE(SM)))) INFO = MAX(INFO, (J - 1) * N + I)
           S = MAX(S, SC, SM)
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF
