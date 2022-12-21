  INFO = 0
  IF (LDW .LT. M) INFO = -6
  IF (LDG .LT. M) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (M .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN

  DO J = 1, N
     DO I = 1, M
        H = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
        IF (.NOT. (H .LE. HUGE(H))) THEN
           INFO = -3
           RETURN
        END IF
        W(I,J) = H
     END DO
  END DO
