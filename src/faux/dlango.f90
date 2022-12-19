!>@brief \b DLANGO computes \f$S=\|G\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'A'},\mathrm{'a'}\}\f$ or \f$S=\|\mathop{\mathrm{off}}(G)\|_F\f$ for \f$\mathrm{O}\in\{\mathrm{'O'},\mathrm{'o'}\}\f$ of a square double precision real matrix \f$G\f$ of order \f$N\f$.
PURE SUBROUTINE DLANGO(O, N, G, LDG, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  INTERFACE
     PURE FUNCTION DLANGE(NORM, M, N, A, LDA, WORK)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: NORM
       INTEGER, INTENT(IN) :: M, N, LDA
       REAL(KIND=REAL64), INTENT(IN) :: A(LDA,*)
       ! a dirty trick to preserve purity since WORK should not be referenced here
       REAL(KIND=REAL64), INTENT(IN) :: WORK
       REAL(KIND=REAL64) :: DLANGE
     END FUNCTION DLANGE
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE DLASSQ(N, X, INCX, SC, SM)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, INCX
       REAL(KIND=REAL64), INTENT(IN) :: X(*)
       REAL(KIND=REAL64), INTENT(INOUT) :: SC, SM
     END SUBROUTINE DLASSQ
  END INTERFACE

  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL64), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL64), INTENT(OUT) :: S
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL64) :: SC, SM
  INTEGER :: J

  S = 0.0_REAL64
  INFO = 0
  IF (LDG .LT. MAX(N, 0)) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  SELECT CASE (O)
  CASE ('A','a')
     S = DLANGE('F', N, N, G, LDG, SC)
  CASE ('O','o')
     IF (N .GE. 2) THEN
        SC = 0.0_REAL64
        SM = 1.0_REAL64
        CALL DLASSQ(N-1, G(2,1), 1, SC, SM)
        DO J = 2, N-1
           CALL DLASSQ(J-1, G(1,J), 1, SC, SM)
           CALL DLASSQ(N-J, G(J+1,J), 1, SC, SM)
        END DO
        CALL DLASSQ(N-1, G(1,N), 1, SC, SM)
        S = SC * SQRT(SM)
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE DLANGO
