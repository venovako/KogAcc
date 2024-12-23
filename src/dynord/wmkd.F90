!>@brief \b WMKD builds D from G.
SUBROUTINE WMKD(N, G, LDG, D, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
#ifdef CR_MATH
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  INTERFACE
     PURE SUBROUTINE XENC(E, S, P, Q) BIND(C,NAME='pvn_djs_xenc_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       REAL(KIND=c_long_double), INTENT(OUT) :: E
       REAL(KIND=REAL64), INTENT(IN) :: S
       INTEGER, INTENT(IN) :: P, Q
     END SUBROUTINE XENC
  END INTERFACE

  REAL(KIND=c_long_double), PARAMETER :: WZERO = 0.0_c_long_double
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64

  INTEGER, INTENT(IN) :: N, LDG, O(2,*)
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: INFO

  REAL(KIND=c_long_double) :: W
  REAL(KIND=REAL64) :: H
  INTEGER :: K, L, P, Q

  L = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (N .GT. 32) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (MOD(N, 2) .EQ. 0) THEN
     INFO = (N / 2) * (N - 1)
  ELSE ! N odd
     INFO = N * ((N - 1) / 2)
  END IF

  W = WZERO
  DO K = 1, INFO
     P = O(1,K)
     Q = O(2,K)
     H = CR_HYPOT(CR_HYPOT(REAL(G(Q,P)), AIMAG(G(Q,P))), CR_HYPOT(REAL(G(P,Q)), AIMAG(G(P,Q))))
     IF ((H .GT. ZERO) .OR. (AIMAG(G(P,P)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(P,P))) .NE. ONE) .OR. &
          (AIMAG(G(Q,Q)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(Q,Q))) .NE. ONE) .OR. &
          (REAL(G(P,P)) .LT. REAL(G(Q,Q)))) THEN
        CALL XENC(D(K), H, P, Q)
        W = MAX(W, D(K))
     ELSE ! no transformation
        D(K) = -H
     END IF
  END DO
  D(INFO+1) = W
END SUBROUTINE WMKD
