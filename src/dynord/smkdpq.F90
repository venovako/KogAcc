!>@brief \b SMKDPQ builds N/2 pivot index pairs for the next transformation of G.
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
  INTEGER :: I, J, K, L, M, P, Q

  L = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (N .GT. 16384) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  M = (N * (N - 1)) / 2
  ! build D and determine its largest element
  W = MONE
  IF (L .EQ. 0) THEN
     DO K = 1, M
        P = O(1,K)
        Q = O(2,K)
        IF (G(Q,P) .EQ. ZERO) THEN
           H = ABS(G(P,Q))
        ELSE IF (G(P,Q) .EQ. ZERO) THEN
           H = ABS(G(Q,P))
        ELSE ! none are 0
           H = CR_HYPOT(G(Q,P), G(P,Q))
        END IF
        IF ((H .NE. ZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
           !DIR$ FORCEINLINE
           D(K) = DENC(H, P, Q)
           W = MAX(W, D(K))
        ELSE ! no transformation
           D(K) = MONE
        END IF
     END DO
  ELSE ! OpenMP
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,M,O) PRIVATE(H,K,P,Q) REDUCTION(MAX:W)
     DO K = 1, M
        P = O(1,K)
        Q = O(2,K)
        IF (G(Q,P) .EQ. ZERO) THEN
           H = ABS(G(P,Q))
        ELSE IF (G(P,Q) .EQ. ZERO) THEN
           H = ABS(G(Q,P))
        ELSE ! none are 0
           H = CR_HYPOT(G(Q,P), G(P,Q))
        END IF
        IF ((H .NE. ZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
           !DIR$ FORCEINLINE
           D(K) = DENC(H, P, Q)
           W = MAX(W, D(K))
        ELSE ! no transformation
           D(K) = MONE
        END IF
     END DO
     !$OMP END PARALLEL DO
  END IF
  IF (W .LE. WZERO) RETURN

  ! find the remaining pivots
  IF (L .EQ. 0) THEN
     L = N / 2
     DO INFO = 1, L
        !DIR$ FORCEINLINE
        CALL DDEC(W, P, Q)
        K = M + INFO
        O(1,K) = P
        O(2,K) = Q
        IF (INFO .GE. L) EXIT
        W = MONE
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              !DIR$ FORCEINLINE
              CALL DDEC(D(K), I, J)
              IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
                 W = MAX(W, D(K))
              ELSE ! colliding
                 D(K) = MONE
              END IF
           END IF
        END DO
        IF (W .LE. WZERO) EXIT
     END DO
  ELSE ! OpenMP
     L = N / 2
     DO INFO = 1, L
        !DIR$ FORCEINLINE
        CALL DDEC(W, P, Q)
        K = M + INFO
        O(1,K) = P
        O(2,K) = Q
        IF (INFO .GE. L) EXIT
        W = MONE
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W)
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              !DIR$ FORCEINLINE
              CALL DDEC(D(K), I, J)
              IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
                 W = MAX(W, D(K))
              ELSE ! colliding
                 D(K) = MONE
              END IF
           END IF
        END DO
        !$OMP END PARALLEL DO
        IF (W .LE. WZERO) EXIT
     END DO
  END IF

CONTAINS

  PURE FUNCTION DENC(S, P, Q)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64
    IMPLICIT NONE
    REAL(KIND=REAL32), INTENT(IN) :: S
    INTEGER, INTENT(IN) :: P, Q
    REAL(KIND=REAL64) :: DENC
    DENC = TRANSFER(&
         IOR(TRANSFER(REAL(S,REAL64), 0_INT64), IOR(ISHFT(INT(IAND(P-1,16383),INT64),14), INT(IAND(Q-1,16383),INT64))), DENC)
  END FUNCTION DENC
  PURE SUBROUTINE DDEC(D, P, Q)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64
    IMPLICIT NONE
    REAL(KIND=REAL64), INTENT(IN) :: D
    INTEGER, INTENT(OUT) :: P, Q
    INTEGER(KIND=INT64) :: J
    J = TRANSFER(D, J)
    Q = INT(IAND(J, 16383_INT64)) + 1
    J = ISHFT(J, -14)
    P = INT(IAND(J, 16383_INT64)) + 1
  END SUBROUTINE DDEC
END SUBROUTINE SMKDPQ
