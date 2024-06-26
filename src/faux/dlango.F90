!>@brief \b DLANGO computes an approximation of the norm of G.
SUBROUTINE DLANGO(N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL64), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL64), INTENT(OUT) :: S
  INTEGER, INTENT(INOUT) :: INFO
  REAL(KIND=REAL64) :: SC
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
           SC = ABS(G(I,J))
#ifndef NDEBUG
           IF (.NOT. (SC .LE. HUGE(SC))) THEN
              S = SC
              INFO = (J - 1) * N + I
              RETURN
           END IF
#endif
           S = MAX(S, SC)
        END DO
     END DO
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
  ELSE ! OpenMP
#ifdef NDEBUG
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC) SHARED(G,N) REDUCTION(MAX:S)
     DO J = 1, N
        DO I = 1, N
           SC = ABS(G(I,J))
           S = MAX(S, SC)
        END DO
     END DO
     !$OMP END PARALLEL DO
     IF (.NOT. (S .LE. HUGE(S))) INFO = 1
#else
     !$OMP PARALLEL DO DEFAULT(NONE) PRIVATE(I,J,SC) SHARED(G,N) REDUCTION(MAX:S,INFO)
     DO J = 1, N
        DO I = 1, N
           SC = ABS(G(I,J))
           IF (.NOT. (SC .LE. HUGE(SC))) THEN
              INFO = MAX(INFO, (J - 1) * N + I)
           ELSE ! SC finite
              INFO = MAX(INFO, 0)
           END IF
           S = MAX(S, SC)
        END DO
     END DO
     !$OMP END PARALLEL DO
#endif
  END IF
END SUBROUTINE DLANGO
