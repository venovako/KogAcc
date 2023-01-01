  I = INFO
  INFO = 0
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. N) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. M) RETURN

  IF (I .EQ. 0) THEN
     DO J = 1, N
        DO I = N+1, M
           G(I,J) = ZERO
        END DO
     END DO
     DO J = N+1, M
        DO I = 1, M
           G(I,J) = ZERO
        END DO
     END DO
  ELSE ! OpenMP
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,N,G)
     DO J = 1, N
        DO I = N+1, M
           G(I,J) = ZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,N,G)
     DO J = N+1, M
        DO I = 1, M
           G(I,J) = ZERO
        END DO
     END DO
     !$OMP END PARALLEL DO
  END IF
