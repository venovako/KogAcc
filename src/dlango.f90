!>@brief \b DLANGO computes \f$\|\mathop{\mathrm{off}}(G)\|_F\f$ of a square double precision real matrix \f$G\f$ of order \f$N\f$.
FUNCTION DLANGO(N, G, LDG)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL64), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL64) :: DLANGO
  REAL(KIND=REAL64) :: SC, SM
  INTEGER :: J
  EXTERNAL :: DLASSQ
  IF (LDG .LT. N) THEN
     DLANGO = -3.0_REAL64
     RETURN
  END IF
  IF (N .LT. 0) THEN
     DLANGO = -1.0_REAL64
     RETURN
  END IF
  IF (N .LT. 2) THEN
     DLANGO = 0.0_REAL64
     RETURN
  END IF
  SC = 0.0_REAL64
  SM = 1.0_REAL64
  CALL DLASSQ(N-1, G(2,1), 1, SC, SM)
  DO J = 2, N-1
     CALL DLASSQ(J-1, G(1,J), 1, SC, SM)
     CALL DLASSQ(N-J, G(J+1,J), 1, SC, SM)
  END DO
  CALL DLASSQ(N-1, G(1,N), 1, SC, SM)
  DLANGO = SC * SQRT(SM)
END FUNCTION DLANGO
