!>@brief \b YLANGO computes \f$S=\|G\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'A'},\mathrm{'a'}\}\f$ or \f$S=\|\mathop{\mathrm{off}}(G)\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'O'},\mathrm{'o'}\}\f$ or \f$S=\|G\|_{\max}\f$ for \f$\mathrm{O}\in\{\mathrm{'M'},\mathrm{'m'}\}\f$ of a square quadruple precision complex matrix \f$G\f$ of order \f$N\f$.
PURE SUBROUTINE YLANGO(O, N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE

  REAL(KIND=REAL128), PARAMETER :: ZERO = 0.0_REAL128
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL128), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL128), INTENT(OUT) :: S
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J

  S = ZERO
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN

  ! this is to be used for debugging and illustration purposes only
  SELECT CASE (O)
  CASE ('A','a')
     DO J = 1, N
        DO I = 1, N
           S = HYPOT(S, REAL(G(I,J)))
           S = HYPOT(S, AIMAG(G(I,J)))
        END DO
     END DO
  CASE ('M','m')
     DO J = 1, N
        DO I = 1, N
           S = MAX(S, HYPOT(REAL(G(I,J)), AIMAG(G(I,J))))
        END DO
     END DO
  CASE ('N','n')
     DO J = 1, N
        DO I = 1, N
           S = MAX(S, ABS(REAL(G(I,J))), ABS(AIMAG(G(I,J))))
        END DO
     END DO
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
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE YLANGO
