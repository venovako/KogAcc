!>@brief \b DPQSORT sorts the arrays \f$AW,AP,AQ\f$ by the OpenMP-parallel merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the WPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE DPQSORT(WPQCMP, N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL64), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=REAL64), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: WPQCMP, DPQMRG
  INTEGER :: I, J, K, L, M, X, Y
  LOGICAL :: FLIP
  INFO = 0
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  FLIP = .FALSE.
  L = 1
  DO WHILE (L .LT. N)
     K = L
     L = L * 2
     M = 0
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(AW,AP,AQ,BW,BP,BQ,K,L,N,FLIP) PRIVATE(J,X,Y) REDUCTION(+:INFO,M)
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
           CALL DPQMRG(WPQCMP, X, Y, BW(I), BP(I), BQ(I), BW(J), BP(J), BQ(J), AW(I), AP(I), AQ(I), J)
        ELSE ! the initial direction
           CALL DPQMRG(WPQCMP, X, Y, AW(I), AP(I), AQ(I), AW(J), AP(J), AQ(J), BW(I), BP(I), BQ(I), J)
        END IF
        IF (J .LT. 0) THEN
           M = M + 1
        ELSE ! all OK
           INFO = INFO + J
        END IF
     END DO
     !$OMP END PARALLEL DO
     IF (M .GT. 0) THEN
        INFO = -9
        RETURN
     END IF
     FLIP = .NOT. FLIP
  END DO
  IF (FLIP) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(AW,AP,AQ,BW,BP,BQ,N)
     DO I = 1, N
        AW(I) = BW(I)
        AP(I) = BP(I)
        AQ(I) = BQ(I)
     END DO
     !$OMP END PARALLEL DO
  END IF
END SUBROUTINE DPQSORT
