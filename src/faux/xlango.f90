!>@brief \b XLANGO computes approximations of the various norms of G or off(G).
SUBROUTINE XLANGO(O, N, G, LDG, S, INFO)
  IMPLICIT NONE

  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=10), INTENT(IN) :: G(N,LDG)
  REAL(KIND=10), INTENT(OUT) :: S
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J

  S = ZERO
  I = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .EQ. 0) RETURN

  SELECT CASE (O)
  CASE ('F','f')
     DO J = 1, N
        DO I = 1, N
           S = HYPOT(S, G(I,J))
        END DO
     END DO
  CASE ('M','N','m','n')
     IF (I .EQ. 0) THEN
        DO J = 1, N
           DO I = 1, N
              S = MAX(S, ABS(G(I,J)))
           END DO
        END DO
     ELSE ! OpenMP
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,N) REDUCTION(MAX:S)
        DO J = 1, N
           DO I = 1, N
              S = MAX(S, ABS(G(I,J)))
           END DO
        END DO
        !$OMP END PARALLEL DO
     END IF
  CASE ('O','o')
     DO J = 1, N
        DO I = 1, J-1
           S = HYPOT(S, G(I,J))
        END DO
        DO I = J+1, N
           S = HYPOT(S, G(I,J))
        END DO
     END DO
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE XLANGO
