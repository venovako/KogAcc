  ! find the remaining pivots
  L = INFO
  INFO = 0
#ifndef NDEBUG
  IF (N .LT. 0) INFO = -1
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
#endif
  IF (N .EQ. 0) RETURN
  IF (M .EQ. 0) RETURN
  W = D(M+1)
  IF (W .LE. WZERO) RETURN

  R = N / 2
  IF (L .EQ. 0) THEN
     L = M
     DO INFO = 1, R
        P = 0; Q = 0
        CALL XDEC(W, P, Q)
        O(1,INFO) = P
        O(2,INFO) = Q
        IF (INFO .GE. R) EXIT
        W = WZERO
        K = 1
        DO WHILE (K .LE. L)
           IF (D(K) .GT. WZERO) THEN
              I = 0; J = 0
              CALL XDEC(D(K), I, J)
              IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
                 W = MAX(W, D(K))
                 K = K + 1
              ELSE ! colliding
                 D(K) = D(L)
                 L = L - 1
              END IF
           ELSE ! can be skipped
              D(K) = D(L)
              L = L - 1
           END IF
        END DO
        IF (W .LE. WZERO) EXIT
     END DO
  ELSE ! L .NE. 0
     DO INFO = 1, R
        P = 0; Q = 0
        CALL XDEC(W, P, Q)
        O(1,INFO) = P
        O(2,INFO) = Q
        IF (INFO .GE. R) EXIT
        W = WZERO
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W)
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              I = 0; J = 0
              CALL XDEC(D(K), I, J)
              IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
                 W = MAX(W, D(K))
              ELSE ! colliding
                 D(K) = -D(K)
              END IF
           END IF
        END DO
        !$OMP END PARALLEL DO
        IF (W .LE. WZERO) EXIT
     END DO
  END IF
