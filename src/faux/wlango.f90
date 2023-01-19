!>@brief \b WLANGO computes approximations of the various norms of G or off(G).
SUBROUTINE WLANGO(O, N, G, LDG, S, INFO)
  IMPLICIT NONE

  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=10), INTENT(IN) :: G(N,LDG)
  REAL(KIND=10), INTENT(OUT) :: S
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=10) :: SC, SM
  INTEGER :: I, J

  S = ZERO
  I = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN

  SELECT CASE (O)
  CASE ('F','f')
     DO J = 1, N
        DO I = 1, N
           S = HYPOT(S, REAL(G(I,J)))
           S = HYPOT(S, AIMAG(G(I,J)))
        END DO
     END DO
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
  CASE ('M','m')
     IF (I .EQ. 0) THEN
        DO J = 1, N
           DO I = 1, N
              SC = HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
              IF (.NOT. (SC .LE. HUGE(SC))) THEN
                 S = SC
                 INFO = (J - 1) * N + I
                 RETURN
              END IF
              S = MAX(S, SC)
           END DO
        END DO
     ELSE ! OpenMP
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,N,SC) REDUCTION(MAX:S,INFO)
        DO J = 1, N
           DO I = 1, N
              SC = HYPOT(REAL(G(I,J)), AIMAG(G(I,J)))
              IF (.NOT. (SC .LE. HUGE(SC))) THEN
                 INFO = MAX(INFO, (J - 1) * N + I)
              ELSE ! SC finite
                 INFO = MAX(INFO, 0)
              END IF
              S = MAX(S, SC)
           END DO
        END DO
        !$OMP END PARALLEL DO
     END IF
  CASE ('N','n')
     IF (I .EQ. 0) THEN
        DO J = 1, N
           DO I = 1, N
              SC = ABS(REAL(G(I,J)))
              IF (.NOT. (SC .LE. HUGE(SC))) THEN
                 S = SC
                 INFO = (J - 1) * N + I
                 RETURN
              END IF
              SM = ABS(AIMAG(G(I,J)))
              IF (.NOT. (SM .LE. HUGE(SM))) THEN
                 S = SM
                 INFO = (J - 1) * N + I
                 RETURN
              END IF
              S = MAX(S, SC, SM)
           END DO
        END DO
     ELSE ! OpenMP
        !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J) SHARED(G,N,SC,SM) REDUCTION(MAX:S,INFO)
        DO J = 1, N
           DO I = 1, N
              SC = ABS(REAL(G(I,J)))
              SM = ABS(AIMAG(G(I,J)))
              IF ((.NOT. (SC .LE. HUGE(SC))) .OR. (.NOT. (SM .LE. HUGE(SM)))) THEN
                 INFO = MAX(INFO, (J - 1) * N + I)
              ELSE ! SC and SM finite
                 INFO = MAX(INFO, 0)
              END IF
              S = MAX(S, SC, SM)
           END DO
        END DO
        !$OMP END PARALLEL DO
     END IF
  CASE ('O','o')
     DO J = 1, N
        DO I = 1, J-1
           S = HYPOT(S, REAL(G(I,J)))
           S = HYPOT(S, AIMAG(G(I,J)))
        END DO
        DO I = J+1, N
           S = HYPOT(S, REAL(G(I,J)))
           S = HYPOT(S, AIMAG(G(I,J)))
        END DO
     END DO
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE WLANGO
