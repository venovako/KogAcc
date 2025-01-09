  IF (ALPHA .EQ. ZERO) THEN
     IF (BETA .EQ. ZERO) THEN
        DO I = 1, M
           DO J = 1, N
              C(I,J) = ZERO
           END DO
        END DO
     ELSE IF (BETA .EQ. ONE) THEN
        RETURN
     ELSE ! general BETA
        DO I = 1, M
           DO J = 1, N
              C(I,J) = KMUL(BETA, C(I,J))
           END DO
        END DO
     END IF
  ELSE IF (ALPHA .EQ. ONE) THEN
     IF (BETA .EQ. ZERO) THEN
        DO I = 1, M
           DO J = 1, N
              C(I,J) = ZERO
              DO L = 1, K
                 C(I,J) = KFMA(A(I,L), B(L,J), C(I,J))
              END DO
           END DO
        END DO
     ELSE IF (BETA .EQ. ONE) THEN
        DO I = 1, M
           DO J = 1, N
              DO L = 1, K
                 C(I,J) = KFMA(A(I,L), B(L,J), C(I,J))
              END DO
           END DO
        END DO
     ELSE ! general BETA
        DO I = 1, M
           DO J = 1, N
              C(I,J) = KMUL(BETA, C(I,J))
              DO L = 1, K
                 C(I,J) = KFMA(A(I,L), B(L,J), C(I,J))
              END DO
           END DO
        END DO
     END IF
  ELSE ! general ALPHA
     IF (BETA .EQ. ZERO) THEN
        DO I = 1, M
           DO J = 1, N
              C(I,J) = ZERO
              DO L = 1, K
                 C(I,J) = KFMA(KMUL(ALPHA, A(I,L)), B(L,J), C(I,J))
              END DO
           END DO
        END DO
     ELSE IF (BETA .EQ. ONE) THEN
        DO I = 1, M
           DO J = 1, N
              DO L = 1, K
                 C(I,J) = KFMA(KMUL(ALPHA, A(I,L)), B(L,J), C(I,J))
              END DO
           END DO
        END DO
     ELSE ! general BETA
        DO I = 1, M
           DO J = 1, N
              C(I,J) = KMUL(BETA, C(I,J))
              DO L = 1, K
                 C(I,J) = KFMA(KMUL(ALPHA, A(I,L)), B(L,J), C(I,J))
              END DO
           END DO
        END DO
     END IF
  END IF
