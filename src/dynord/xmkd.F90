!>@brief \b XMKD builds (an approximate) D from G.
SUBROUTINE XMKD(N, G, LDG, D, O, INFO)
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
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: INFO

  REAL(KIND=c_long_double) :: W, HH
  REAL(KIND=REAL64) :: H
  INTEGER :: K, L, P, Q

  L = INFO
  INFO = 0
#ifndef NDEBUG
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (L .GE. 0) THEN
#ifdef HAVE_UNSIGNED
     IF (N .GT. 256) INFO = -1
#else
     IF (N .GT. 128) INFO = -1
#endif
  ELSE ! L .LT. 0
#ifdef CLS
     IF (N .GT. 1073741824) INFO = -1
#else
     IF (N .GT. 32) INFO = -1
#endif
  END IF
  IF (INFO .NE. 0) RETURN
#endif
  IF (MOD(N, 2) .EQ. 0) THEN
     INFO = (N / 2) * (N - 1)
  ELSE ! N odd
     INFO = N * ((N - 1) / 2)
  END IF

  W = WZERO
  IF (L .GE. 0) THEN
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,O,INFO) PRIVATE(HH,K,P,Q) REDUCTION(MAX:W) IF(L .NE. 0)
     DO K = 1, INFO
        P = O(1,K)
        Q = O(2,K)
        IF (G(Q,P) .EQ. ZERO) THEN
           HH = ABS(G(P,Q))
        ELSE IF (G(P,Q) .EQ. ZERO) THEN
           HH = ABS(G(Q,P))
        ELSE ! none are 0
           HH = G(Q,P)
           D(K) = G(P,Q)
           HH = HH * HH + D(K) * D(K)
        END IF
        IF ((HH .GT. WZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
           D(K) = XENCAP(HH, P, Q)
           W = MAX(W, D(K))
        ELSE ! no transformation
           D(K) = -HH
        END IF
     END DO
     !$OMP END PARALLEL DO
  ELSE ! L .LT. 0
     L = L + 1
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,O,INFO) PRIVATE(H,K,P,Q) REDUCTION(MAX:W) IF(L .NE. 0)
     DO K = 1, INFO
        P = O(1,K)
        Q = O(2,K)
        IF (G(Q,P) .EQ. ZERO) THEN
           H = ABS(G(P,Q))
        ELSE IF (G(P,Q) .EQ. ZERO) THEN
           H = ABS(G(Q,P))
        ELSE ! none are 0
           H = CR_HYPOT(G(Q,P), G(P,Q))
        END IF
        IF ((H .GT. ZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
           CALL XENC(D(K), H, P, Q)
           W = MAX(W, D(K))
        ELSE ! no transformation
           D(K) = -H
        END IF
     END DO
     !$OMP END PARALLEL DO
  END IF
  D(INFO+1) = W
CONTAINS
#include "xencap.F90"
END SUBROUTINE XMKD
