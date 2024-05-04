!>@brief \b WLANGO computes an approximation of the norm of G.
SUBROUTINE WLANGO(N, G, LDG, S, INFO)
  IMPLICIT NONE

  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=10), INTENT(IN) :: G(N,LDG)
  REAL(KIND=10), INTENT(OUT) :: S
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=10) :: SC, SM
  INTEGER :: I, J

  S = ZERO
  I = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN

  IF (I .EQ. 0) THEN
     DO J = 1, N
        DO I = 1, N
           SC = ABS(REAL(G(I,J)))
#ifndef NDEBUG
           IF (.NOT. (SC .LE. HUGE(SC))) THEN
              S = SC
              INFO = (J - 1) * N + I
              RETURN
           END IF
#endif
           SM = ABS(AIMAG(G(I,J)))
#ifndef NDEBUG
           IF (.NOT. (SM .LE. HUGE(SM))) THEN
              S = SM
              INFO = (J - 1) * N + I
              RETURN
           END IF
#endif
           S = MAX(S, SC, SM)
        END DO
     END DO
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
  ELSE ! OpenMP
#ifdef NDEBUG
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC,SM) SHARED(G,N) REDUCTION(MAX:S)
     DO J = 1, N
        DO I = 1, N
           SC = ABS(REAL(G(I,J)))
           SM = ABS(AIMAG(G(I,J)))
           S = MAX(S, SC, SM)
        END DO
     END DO
     !$OMP END PARALLEL DO
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
#else
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC,SM) SHARED(G,N) REDUCTION(MAX:S,INFO)
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
#endif
  END IF
END SUBROUTINE WLANGO
