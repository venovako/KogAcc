!>@brief \b DBPACK packs G into GB.
SUBROUTINE DBPACK(N, G, LDG, B, GB, LDB, NB, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, LDG, B, LDB, NB, O(2,NB)
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL64), INTENT(OUT) :: GB(LDB,2*B,NB)
  INTEGER, INTENT(INOUT) :: INFO

  INTEGER :: I, J, K, L, M, P, Q

  L = INFO
  INFO = 0
  M = 2 * B
  IF (L .LT. 0) INFO = -9
  IF (NB .LT. 0) INFO = -7
  IF (LDB .LT. M) INFO = -6
  IF (B .LT. 2) INFO = -4
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. (NB * M)) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (NB .EQ. 0) RETURN

  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,G,B,GB,LDB,NB,O) PRIVATE(I,J,K,L,M,P,Q) REDUCTION(MIN:INFO) IF(L .NE. 0)
  DO K = 1, NB
     P = O(1,K)
     Q = O(2,K)
     L = (P - 1) * B
     M = (Q - 1) * B
     IF ((M .LT. 0) .OR. (M .GE. N)) THEN
        INFO = MIN(INFO, -8)
     ELSE IF ((L .LT. 0) .OR. (L .GE. N)) THEN
        INFO = MIN(INFO, -5)
     ELSE IF (M .LE. L) THEN
        INFO = MIN(INFO, -2)
     ELSE ! all OK
        INFO = MIN(INFO, 0)
        L = (P - 1) * B
        M = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(I,J,K) = G(L+I,M+J)
           END DO
        END DO
        L = (Q - 1) * B
        M = (P - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(B+I,J,K) = G(L+I,M+J)
           END DO
           !DIR$ VECTOR ALWAYS
           DO I = 2*B+1, LDB
              GB(I,J,K) = 0.0_REAL64
           END DO
        END DO
        L = (P - 1) * B
        M = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(I,B+J,K) = G(L+I,M+J)
           END DO
        END DO
        L = (Q - 1) * B
        M = (Q - 1) * B
        DO J = 1, B
           !DIR$ VECTOR ALWAYS
           DO I = 1, B
              GB(B+I,B+J,K) = G(L+I,M+J)
           END DO
           !DIR$ VECTOR ALWAYS
           DO I = 2*B+1, LDB
              GB(I,B+J,K) = 0.0_REAL64
           END DO
        END DO
     END IF
  END DO
  !$OMP END PARALLEL DO
END SUBROUTINE DBPACK
