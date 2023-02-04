SUBROUTINE SMKDPQ(N, G, LDG, D, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  IMPLICIT NONE

#ifdef CR_MATH
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
#else
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypotf')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_float
       REAL(KIND=c_float), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_float) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  REAL(KIND=REAL64), PARAMETER :: WZERO = 0.0_REAL64, MONE = -1.0_REAL64
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32, ONE = 1.0_REAL32

  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL32), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL64), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: O(2,*), INFO

  REAL(KIND=REAL64) :: W
  REAL(KIND=REAL32) :: H
  INTEGER :: I, J, K, L, M, P, Q, S, T, X

  I = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  M = (N * (N - 1)) / 2
  ! build D and determine its largest element
  L = 0
  W = MONE
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,L,M,O) PRIVATE(H,J,K,P,Q) REDUCTION(MAX:W)
  DO J = 1, M
     P = O(1,J)
     Q = O(2,J)
     IF (G(Q,P) .EQ. ZERO) THEN
        H = ABS(G(P,Q))
     ELSE IF (G(P,Q) .EQ. ZERO) THEN
        H = ABS(G(Q,P))
     ELSE ! none are 0
        H = CR_HYPOT(G(Q,P), G(P,Q))
     END IF
     IF ((H .NE. ZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
        !$OMP ATOMIC CAPTURE SEQ_CST
        L = L + 1
        K = L
        !$OMP END ATOMIC
        D(K) = SENC(H, P, Q)
        W = MAX(W, D(K))
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (W .LE. WZERO) RETURN

  ! find the remaining pivots
  DO INFO = 1, N/2
     CALL DDEC(W, P, Q)
     K = M + INFO
     O(1,K) = P
     O(2,K) = Q
     IF (L .LE. 0) EXIT
     IF (MOD(INFO, 2) .EQ. 0) THEN
        S = M
        T = 0
     ELSE ! INFO odd
        S = 0
        T = M
     END IF
     X = 0
     W = MONE
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,L,P,Q,S,T,X) PRIVATE(I,J,K) REDUCTION(MAX:W)
     DO K = 1, L
        CALL DDEC(D(S+K), I, J)
        IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
           !$OMP ATOMIC CAPTURE SEQ_CST
           X = X + 1
           I = X
           !$OMP END ATOMIC
           J = S + K
           D(T+I) = D(J)
           W = MAX(W, D(J))
        END IF
     END DO
     !$OMP END PARALLEL DO
     IF (W .LE. WZERO) EXIT
     L = X
  END DO

CONTAINS

  PURE FUNCTION SENC(S, P, Q)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64
    IMPLICIT NONE
    REAL(KIND=REAL32), INTENT(IN) :: S
    INTEGER, INTENT(IN) :: P, Q
    REAL(KIND=REAL64) :: SENC
    SENC = TRANSFER(&
         IOR(TRANSFER(REAL(S, REAL64), 0_INT64), IOR(ISHFT(INT(IAND(P, 16383), INT64), 14), INT(IAND(Q, 16383), INT64))), SENC)
  END FUNCTION SENC
  PURE SUBROUTINE DDEC(D, P, Q)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64
    IMPLICIT NONE
    REAL(KIND=REAL64), INTENT(IN) :: D
    INTEGER, INTENT(OUT) :: P, Q
    INTEGER(KIND=INT64) :: J
    J = TRANSFER(D, J)
    Q = IAND(J, 16383_INT64)
    J = ISHFT(J, -14)
    P = IAND(J, 16383_INT64)
  END SUBROUTINE DDEC
END SUBROUTINE SMKDPQ
