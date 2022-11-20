!>@brief \b SLANGO computes \f$S=\|G\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'A'},\mathrm{'a'}\}\f$ or \f$S=\|\mathop{\mathrm{off}}(G)\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'O'},\mathrm{'o'}\}\f$ of a square single precision real matrix \f$G\f$ of order \f$N\f$.
SUBROUTINE SLANGO(O, N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL32), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL32), INTENT(OUT) :: S
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL32) :: SC, SM
  INTEGER :: J
  REAL(KIND=REAL32), EXTERNAL :: SLANGE
  EXTERNAL :: SLASSQ
  S = 0.0_REAL32
  INFO = 0
  IF (LDG .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  SELECT CASE (O)
  CASE ('A','a')
     S = SLANGE('F', N, N, G, LDG, SC)
  CASE ('O','o')
     IF (N .GE. 2) THEN
        SC = 0.0_REAL32
        SM = 1.0_REAL32
        CALL SLASSQ(N-1, G(2,1), 1, SC, SM)
        DO J = 2, N-1
           CALL SLASSQ(J-1, G(1,J), 1, SC, SM)
           CALL SLASSQ(N-J, G(J+1,J), 1, SC, SM)
        END DO
        CALL SLASSQ(N-1, G(1,N), 1, SC, SM)
        S = SC * SQRT(SM)
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE SLANGO
