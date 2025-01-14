!>@brief \b XMKDAP builds an approximate D from G.
PURE SUBROUTINE XMKDAP(N, G, LDG, D, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8, REAL64
  IMPLICIT NONE

  REAL(KIND=c_long_double), PARAMETER :: WZERO = 0.0_c_long_double
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64

  INTEGER, INTENT(IN) :: N, LDG, O(2,*)
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: INFO

  REAL(KIND=c_long_double) :: W, H
  INTEGER(KIND=INT8) :: B(16)
  EQUIVALENCE(H, B)
  INTEGER :: K, L, P, Q

  L = INFO
  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (N .GE. 128) INFO = -1
  IF (INFO .NE. 0) RETURN
#endif
  IF (MOD(N, 2) .EQ. 0) THEN
     INFO = (N / 2) * (N - 1)
  ELSE ! N odd
     INFO = N * ((N - 1) / 2)
  END IF

  W = WZERO
  DO K = 1, INFO
     P = O(1,K)
     Q = O(2,K)
     IF (G(Q,P) .EQ. ZERO) THEN
        H = ABS(G(P,Q))
     ELSE IF (G(P,Q) .EQ. ZERO) THEN
        H = ABS(G(Q,P))
     ELSE ! none are 0
        H = G(Q,P)
        D(K) = G(P,Q)
        H = H * H + D(K) * D(K)
     END IF
     IF ((H .GT. WZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
        B(1) = INT(Q, INT8)
        B(2) = INT(P, INT8)
        D(K) = H
        W = MAX(W, D(K))
     ELSE ! no transformation
        D(K) = -H
     END IF
  END DO
  D(INFO+1) = W
END SUBROUTINE XMKDAP
