  Z = INFO
  INFO = 0
#ifndef NDEBUG
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
#endif
  FLIP = .FALSE.
  L = 1

  DO WHILE (L .LT. N)
     K = L
     L = L * 2
     M = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(AW,AP,AQ,BW,BP,BQ,K,L,N,FLIP) PRIVATE(I,J,X,Y) REDUCTION(+:INFO,M) IF(Z .NE. 0)
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
           M = M + 1
        ELSE ! all OK
           INFO = INFO + J
        END IF
     END DO
     !$OMP END PARALLEL DO
     IF (M .GT. 0) THEN
        INFO = -10
        RETURN
     END IF
     FLIP = .NOT. FLIP
  END DO
  IF (FLIP) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(AW,AP,AQ,BW,BP,BQ,N) PRIVATE(I) IF(Z .NE. 0)
     DO I = 1, N
        AW(I) = BW(I)
        AP(I) = BP(I)
        AQ(I) = BQ(I)
     END DO
     !$OMP END PARALLEL DO
  END IF
