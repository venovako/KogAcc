  I = INFO
  INFO = 0
  IF (LDW .LT. M) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,H) SHARED(M,N,G,W) REDUCTION(MIN:INFO) IF(I .NE. 0)
  DO J = 1, N
     INFO = MIN(INFO, 0)
     IF (INFO .EQ. 0) THEN
        DO I = 1, M
           H = ABS(G(I,J))
           IF (.NOT. (H .LE. HUGE(H))) THEN
              INFO = MIN(INFO, -3)
              EXIT
           END IF
           W(I,J) = H
        END DO
     END IF
  END DO
  !$OMP END PARALLEL DO
