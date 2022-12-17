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
        DO J = 1, Q
           DO I = 1, P
              H = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (H .LE. HUGE(H))) THEN
                 INFO = -1
                 RETURN
              END IF
#endif
              W(I,J) = H
           END DO
        END DO
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
        DO J = PB, UP
           DO I = PB, UP
              H = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (H .LE. HUGE(H))) THEN
                 INFO = -1
                 RETURN
              END IF
#endif
              W(I,J) = H
           END DO
           DO I = QB, UQ
              H = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (H .LE. HUGE(H))) THEN
                 INFO = -1
                 RETURN
              END IF
#endif
              W(I,J) = H
           END DO
        END DO
        DO J = QB, UQ
           DO I = PB, UP
              H = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (H .LE. HUGE(H))) THEN
                 INFO = -1
                 RETURN
              END IF
#endif
              W(I,J) = H
           END DO
           DO I = QB, UQ
              H = CR_HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
#ifndef NDEBUG
              IF (.NOT. (H .LE. HUGE(H))) THEN
                 INFO = -1
                 RETURN
              END IF
#endif
              W(I,J) = H
           END DO
        END DO
     END IF
  END IF
