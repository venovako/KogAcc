!>@brief \b ZLANGO computes \f$\|\mathop{\mathrm{off}}(G)\|_F\f$ of a square double precision complex matrix \f$G\f$ of order \f$N\f$.
FUNCTION ZLANGO(N, G, LDG)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL64) :: ZLANGO
  REAL(KIND=REAL64) :: SC, SM
  INTEGER :: J
  EXTERNAL :: ZLASSQ
  IF (LDG .LT. N) THEN
     ZLANGO = -3.0_REAL64
     RETURN
  END IF
  IF (N .LT. 0) THEN
     ZLANGO = -1.0_REAL64
     RETURN
  END IF
  IF (N .LT. 2) THEN
     ZLANGO = 0.0_REAL64
     RETURN
  END IF
  SC = 0.0_REAL64
  SM = 1.0_REAL64
  CALL ZLASSQ(N-1, G(2,1), 1, SC, SM)
  DO J = 2, N-1
     CALL ZLASSQ(J-1, G(1,J), 1, SC, SM)
     CALL ZLASSQ(N-J, G(J+1,J), 1, SC, SM)
  END DO
  CALL ZLASSQ(N-1, G(1,N), 1, SC, SM)
  ZLANGO = SC * SQRT(SM)
END FUNCTION ZLANGO
