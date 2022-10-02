  ! This is the generic part of the sequential bordering routines.
  INFO = 0
  IF (LDG .LT. MAX(M, 0)) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. N) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. M) RETURN
  DO J = 1, N
     DO I = N+1, M
        G(I,J) = ZERO
     END DO
  END DO
  DO J = N+1, M
     DO I = 1, J-1
        G(I,J) = ZERO
     END DO
     G(J,J) = ONE
     DO I = J+1, M
        G(I,J) = ZERO
     END DO
  END DO
