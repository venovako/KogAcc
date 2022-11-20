!>@brief \b SLANGO computes \f$\|\mathop{\mathrm{off}}(G)\|_F\f$ of a square single precision real matrix \f$G\f$ of order \f$N\f$.
FUNCTION SLANGO(N, G, LDG)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL32), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL32) :: SLANGO
  REAL(KIND=REAL32) :: SC, SM
  INTEGER :: J
  EXTERNAL :: SLASSQ
  IF (LDG .LT. N) THEN
     SLANGO = -3.0_REAL32
     RETURN
  END IF
  IF (N .LT. 0) THEN
     SLANGO = -1.0_REAL32
     RETURN
  END IF
  IF (N .LT. 2) THEN
     SLANGO = 0.0_REAL32
     RETURN
  END IF
  SC = 0.0_REAL32
  SM = 1.0_REAL32
  CALL SLASSQ(N-1, G(2,1), 1, SC, SM)
  DO J = 2, N-1
     CALL SLASSQ(J-1, G(1,J), 1, SC, SM)
     CALL SLASSQ(N-J, G(J+1,J), 1, SC, SM)
  END DO
  CALL SLASSQ(N-1, G(1,N), 1, SC, SM)
  SLANGO = SC * SQRT(SM)
END FUNCTION SLANGO
