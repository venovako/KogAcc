!>@brief \b ZLANGO computes \f$S=\|G\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'A'},\mathrm{'a'}\}\f$ or \f$S=\|\mathop{\mathrm{off}}(G)\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'O'},\mathrm{'o'}\}\f$ of a square double precision complex matrix \f$G\f$ of order \f$N\f$.
SUBROUTINE ZLANGO(O, N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL64), INTENT(OUT) :: S
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL64) :: SC, SM
  INTEGER :: J
  REAL(KIND=REAL64), EXTERNAL :: ZLANGE
  EXTERNAL :: ZLASSQ
  S = 0.0_REAL64
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  SELECT CASE (O)
  CASE ('A','a')
     S = ZLANGE('F', N, N, G, LDG, SC)
  CASE ('O','o')
     IF (N .GE. 2) THEN
        SC = 0.0_REAL64
        SM = 1.0_REAL64
        CALL ZLASSQ(N-1, G(2,1), 1, SC, SM)
        DO J = 2, N-1
           CALL ZLASSQ(J-1, G(1,J), 1, SC, SM)
           CALL ZLASSQ(N-J, G(J+1,J), 1, SC, SM)
        END DO
        CALL ZLASSQ(N-1, G(1,N), 1, SC, SM)
        S = SC * SQRT(SM)
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE ZLANGO
