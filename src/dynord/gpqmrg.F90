  ! This is the generic part of the (w,p,q)-arrays merge routines.
  INFO = 0
  IF (N .LT. 0) INFO = -3
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  I = 1
  J = 1
  K = 1
  DO WHILE ((I .LE. M) .AND. (J .LE. N))
     CALL WPQCMP(AW(I), AP(I), AQ(I), BW(J), BP(J), BQ(J), L)
     IF (L .LT. 0) THEN
        XW(K) = AW(I)
        XP(K) = AP(I)
        XQ(K) = AQ(I)
        I = I + 1
     ELSE IF (L .GT. 0) THEN
        XW(K) = BW(J)
        XP(K) = BP(J)
        XQ(K) = BQ(J)
        J = J + 1
        INFO = INFO + 1
     ELSE ! L .EQ. 0
        XW(K) = AW(I)
        XP(K) = AP(I)
        XQ(K) = AQ(I)
        I = I + 1
        K = K + 1
        XW(K) = BW(J)
        XP(K) = BP(J)
        XQ(K) = BQ(J)
        J = J + 1
     END IF
     K = K + 1
  END DO
  DO WHILE (I .LE. M)
     XW(K) = AW(I)
     XP(K) = AP(I)
     XQ(K) = AQ(I)
     I = I + 1
     K = K + 1
  END DO
  DO WHILE (J .LE. N)
     XW(K) = BW(J)
     XP(K) = BP(J)
     XQ(K) = BQ(J)
     J = J + 1
     K = K + 1
  END DO
