  I = INFO
  INFO = 0
#ifndef NDEBUG
  IF (S .LE. -HUGE(S)) INFO = -5
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (S .EQ. 0) RETURN
  INFO = I

  !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,M,N,S) IF(INFO .NE. 0)
  DO J = 1, N
     DO I = 1, M
        G(I,J) = CMPLX(SCALE(REAL(G(I,J)), S), SCALE(AIMAG(G(I,J)), S), K)
     END DO
  END DO
  !$OMP END PARALLEL DO
  INFO = 0
