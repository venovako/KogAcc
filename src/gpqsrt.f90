  ! This is the generic part of the (w,p,q)-arrays sequential sorting routines.
  INFO = 0
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  FLIP = .FALSE.
  L = 1
  DO WHILE (L .LT. N)
     K = L
     L = L * 2
     DO I = 1, N, L
        X = MIN(K, N-I+1)
        IF (X .LT. K) THEN
           J = N
           Y = 0
        ELSE ! the first block is full
           J = I + K
           Y = MIN(K, N-J+1)
           IF (Y .LE. 0) THEN
              J = N
              Y = 0
           END IF
        END IF
        IF (FLIP) THEN
           CALL WPQMRG(WPQCMP, X, Y, BW(I), BP(I), BQ(I), BW(J), BP(J), BQ(J), AW(I), AP(I), AQ(I), J)
        ELSE ! the initial direction
           CALL WPQMRG(WPQCMP, X, Y, AW(I), AP(I), AQ(I), AW(J), AP(J), AQ(J), BW(I), BP(I), BQ(I), J)
        END IF
        IF (J .LT. 0) THEN
           INFO = -9
           RETURN
        ELSE ! all OK
           INFO = INFO + J
        END IF
     END DO
     FLIP = .NOT. FLIP
  END DO
  IF (FLIP) THEN
     DO I = 1, N
        AW(I) = BW(I)
        AP(I) = BP(I)
        AQ(I) = BQ(I)
     END DO
  END IF
