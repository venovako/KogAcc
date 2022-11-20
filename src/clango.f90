!>@brief \b CLANGO computes \f$\|\mathop{\mathrm{off}}(G)\|_F\f$ of a square single precision complex matrix \f$G\f$ of order \f$N\f$.
FUNCTION CLANGO(N, G, LDG)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL32), INTENT(IN) :: G(N,LDG)
  REAL(KIND=REAL32) :: CLANGO
  REAL(KIND=REAL32) :: SC, SM
  INTEGER :: J
  EXTERNAL :: CLASSQ
  IF (LDG .LT. N) THEN
     CLANGO = -3.0_REAL32
     RETURN
  END IF
  IF (N .LT. 0) THEN
     CLANGO = -1.0_REAL32
     RETURN
  END IF
  IF (N .LT. 2) THEN
     CLANGO = 0.0_REAL32
     RETURN
  END IF
  SC = 0.0_REAL32
  SM = 1.0_REAL32
  CALL CLASSQ(N-1, G(2,1), 1, SC, SM)
  DO J = 2, N-1
     CALL CLASSQ(J-1, G(1,J), 1, SC, SM)
     CALL CLASSQ(N-J, G(J+1,J), 1, SC, SM)
  END DO
  CALL CLASSQ(N-1, G(1,N), 1, SC, SM)
  CLANGO = SC * SQRT(SM)
END FUNCTION CLANGO
