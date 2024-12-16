!>@brief \b DMKDPQ builds N/2 pivot index pairs for the next transformation of G.
SUBROUTINE DMKDPQ(N, G, LDG, D, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
  IMPLICIT NONE
#ifdef CR_MATH
  INTERFACE
#ifdef NDEBUG
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
#else
     FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
#endif
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  INTERFACE
     PURE SUBROUTINE QENC(E, S, P, Q) BIND(C,NAME='pvn_djs_qenc_')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
       REAL(KIND=REAL128), INTENT(OUT) :: E
       REAL(KIND=REAL64), INTENT(IN) :: S
       INTEGER, INTENT(IN) :: P, Q
     END SUBROUTINE QENC
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE QDEC(E, P, Q) BIND(C,NAME='pvn_djs_qdec_')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       REAL(KIND=REAL128), INTENT(IN) :: E
       INTEGER, INTENT(OUT) :: P, Q
     END SUBROUTINE QDEC
  END INTERFACE

  REAL(KIND=REAL128), PARAMETER :: WZERO = 0.0_REAL128, MONE = -1.0_REAL128
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64

  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL128), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: O(2,*), INFO

  REAL(KIND=REAL128) :: W
  REAL(KIND=REAL64) :: H
  INTEGER :: I, J, K, L, M, P, Q

  L = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (N .GT. 1073741824) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  M = (N * (N - 1)) / 2
  ! build D and determine its largest element
  W = MONE
  !$OMP PARALLEL DO DEFAULT(NONE) SHARED(G,D,M,O) PRIVATE(H,K,P,Q) REDUCTION(MAX:W) IF(L .NE. 0)
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
     IF ((H .GT. ZERO) .OR. (SIGN(ONE,G(P,P)) .NE. ONE) .OR. (SIGN(ONE,G(Q,Q)) .NE. ONE) .OR. (G(P,P) .LT. G(Q,Q))) THEN
        CALL QENC(D(K), H, P, Q)
        W = MAX(W, D(K))
     ELSE ! no transformation
        D(K) = MONE
     END IF
  END DO
  !$OMP END PARALLEL DO
  IF (W .LE. WZERO) RETURN

  ! find the remaining pivots
  L = N / 2
  DO INFO = 1, L
     P = 0; Q = 0
     CALL QDEC(W, P, Q)
     K = M + INFO
     O(1,K) = P
     O(2,K) = Q
     IF (INFO .GE. L) EXIT
     W = MONE
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W) IF(L .NE. 0)
     DO K = 1, M
        IF (D(K) .GT. WZERO) THEN
           I = 0; J = 0
           CALL QDEC(D(K), I, J)
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
END SUBROUTINE DMKDPQ
