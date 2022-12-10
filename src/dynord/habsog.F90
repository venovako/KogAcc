  INFO = 0
  IF (B .LT. 0) INFO = -7
  IF (Q .LT. 0) INFO = -6
  IF (P .LT. 0) INFO = -5
  IF (LDW .LT. 0) INFO = -4
  IF (LDG .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN

  IF (B .EQ. 0) THEN
     IF (MIN(LDG,LDW) .LT. P) THEN
        INFO = -5
     ELSE ! all OK
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,W,P,Q)
        DO J = 1, Q
           DO I = 1, P
              W(I,J) = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
           END DO
        END DO
        !$OMP END PARALLEL DO
     END IF
  ELSE ! the (P,P), (P,Q), (Q,P), (Q,Q) blocks
     IF (Q .LE. P) THEN
        INFO = -6
     ELSE IF (P .EQ. 0) THEN
        INFO = -5
     ELSE IF (MIN(LDG,LDW) .LT. (P * B)) THEN
        INFO = -5
     ELSE ! all OK
        PB = (P - 1) * B + 1
        QB = (Q - 1) * B + 1
        UP = PB + B - 1
        UQ = QB + B - 1
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,W,PB,QB,UP,UQ)
        DO J = PB, UP
           DO I = PB, UP
              W(I,J) = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
           END DO
           DO I = QB, UQ
              W(I,J) = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
           END DO
        END DO
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,W,PB,QB,UP,UQ)
        DO J = QB, UQ
           DO I = PB, UP
              W(I,J) = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
           END DO
           DO I = QB, UQ
              W(I,J) = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
           END DO
        END DO
        !$OMP END PARALLEL DO
     END IF
  END IF
