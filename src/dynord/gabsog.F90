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
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,H) SHARED(G,W,P,Q) REDUCTION(MIN:INFO)
        DO J = 1, Q
           INFO = MIN(INFO, 0)
           IF (INFO .EQ. 0) THEN
              DO I = 1, P
                 H = ABS(G(I,J))
                 IF (.NOT. (H .LE. HUGE(H))) THEN
                    INFO = MIN(INFO, -1)
                    EXIT
                 END IF
                 W(I,J) = H
              END DO
           END IF
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
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,H) SHARED(G,W,PB,QB,UP,UQ) REDUCTION(MIN:INFO)
        DO J = PB, UP
           INFO = MIN(INFO, 0)
           IF (INFO .EQ. 0) THEN
              DO I = PB, UP
                 H = ABS(G(I,J))
                 IF (.NOT. (H .LE. HUGE(H))) THEN
                    INFO = MIN(INFO, -1)
                    EXIT
                 END IF
                 W(I,J) = H
              END DO
           END IF
           IF (INFO .EQ. 0) THEN
              DO I = QB, UQ
                 H = ABS(G(I,J))
                 IF (.NOT. (H .LE. HUGE(H))) THEN
                    INFO = MIN(INFO, -1)
                    EXIT
                 END IF
                 W(I,J) = H
              END DO
           END IF
        END DO
        !$OMP END PARALLEL DO
        IF (INFO .NE. 0) RETURN
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,H) SHARED(G,W,PB,QB,UP,UQ) REDUCTION(MIN:INFO)
        DO J = QB, UQ
           INFO = MIN(INFO, 0)
           IF (INFO .EQ. 0) THEN
              DO I = PB, UP
                 H = ABS(G(I,J))
                 IF (.NOT. (H .LE. HUGE(H))) THEN
                    INFO = MIN(INFO, -1)
                    EXIT
                 END IF
                 W(I,J) = H
              END DO
           END IF
           IF (INFO .EQ. 0) THEN
              DO I = QB, UQ
                 H = ABS(G(I,J))
                 IF (.NOT. (H .LE. HUGE(H))) THEN
                    INFO = MIN(INFO, -1)
                    EXIT
                 END IF
                 W(I,J) = H
              END DO
           END IF
        END DO
        !$OMP END PARALLEL DO
     END IF
  END IF
