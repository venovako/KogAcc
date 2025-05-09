  I = INFO
  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. N) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (M .EQ. 0) RETURN
  IF (N .EQ. M) RETURN
  INFO = I

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,N,G) PRIVATE(I,J) IF(INFO .NE. 0)
  DO J = 1, N
     !DIR$ VECTOR ALWAYS
     DO I = N+1, M
        G(I,J) = ZERO
     END DO
  END DO
  !$OMP END PARALLEL DO
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(M,N,G) PRIVATE(I,J) IF(INFO .NE. 0)
  DO J = N+1, M
     !DIR$ VECTOR ALWAYS
     DO I = 1, M
        G(I,J) = ZERO
     END DO
  END DO
  !$OMP END PARALLEL DO
  INFO = 0
