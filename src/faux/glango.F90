  S = ZERO
  I = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC) SHARED(G,N) REDUCTION(MAX:S,INFO) IF(I .NE. 0)
  DO J = 1, N
     DO I = 1, N
        SC = ABS(G(I,J))
        IF (.NOT. (SC .LE. HUGE(SC))) THEN
           INFO = MAX(INFO, (J - 1) * N + I)
        ELSE ! SC finite
           INFO = MAX(INFO, 0)
        END IF
        S = MAX(S, SC)
     END DO
  END DO
  !$OMP END PARALLEL DO
