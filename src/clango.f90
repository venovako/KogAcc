!>@brief \b CLANGO computes \f$S=\|G\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'A'},\mathrm{'a'}\}\f$ or \f$S=\|\mathop{\mathrm{off}}(G)\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'O'},\mathrm{'o'}\}\f$ of a square single precision complex matrix \f$G\f$ of order \f$N\f$.
SUBROUTINE CLANGO(O, N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL32), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL32), INTENT(OUT) :: S
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL32) :: SC, SM
  INTEGER :: J
  REAL(KIND=REAL32), EXTERNAL :: CLANGE
  EXTERNAL :: CLASSQ
  S = 0.0_REAL32
  INFO = 0
  IF (LDG .LT. MAX(N, 0)) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  SELECT CASE (O)
  CASE ('A','a')
     S = CLANGE('F', N, N, G, LDG, SC)
  CASE ('O','o')
     IF (N .GE. 2) THEN
        SC = 0.0_REAL32
        SM = 1.0_REAL32
        CALL CLASSQ(N-1, G(2,1), 1, SC, SM)
        DO J = 2, N-1
           CALL CLASSQ(J-1, G(1,J), 1, SC, SM)
           CALL CLASSQ(N-J, G(J+1,J), 1, SC, SM)
        END DO
        CALL CLASSQ(N-1, G(1,N), 1, SC, SM)
        S = SC * SQRT(SM)
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE CLANGO
