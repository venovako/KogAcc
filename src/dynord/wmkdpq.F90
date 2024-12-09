!>@brief \b WMKDPQ builds N/2 pivot index pairs for the next transformation of G.
SUBROUTINE WMKDPQ(N, G, LDG, D, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
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
     PURE SUBROUTINE XENC(E, S, P, Q) BIND(C,NAME='pvn_djs_xenc_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       REAL(KIND=c_long_double), INTENT(OUT) :: E
       REAL(KIND=REAL64), INTENT(IN) :: S
       INTEGER, INTENT(IN) :: P, Q
     END SUBROUTINE XENC
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XDEC(E, P, Q) BIND(C,NAME='pvn_djs_xdec_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN) :: E
       INTEGER, INTENT(OUT) :: P, Q
     END SUBROUTINE XDEC
  END INTERFACE

  REAL(KIND=c_long_double), PARAMETER :: WZERO = 0.0_c_long_double, MONE = -1.0_c_long_double
  REAL(KIND=REAL64), PARAMETER :: ZERO = 0.0_REAL64, ONE = 1.0_REAL64

  INTEGER, INTENT(IN) :: N, LDG
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: D(*)
  INTEGER, INTENT(INOUT) :: O(2,*), INFO

  REAL(KIND=c_long_double) :: W
  REAL(KIND=REAL64) :: H
  INTEGER :: I, J, K, L, M, P, Q

  L = INFO
  INFO = 0
  IF (LDG .LT. N) INFO = -3
  IF (N .LT. 0) INFO = -1
  IF (N .GT. 32) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  M = (N * (N - 1)) / 2
  ! build D and determine its largest element
  W = MONE
  IF (L .EQ. 0) THEN
     DO K = 1, M
        P = O(1,K)
        Q = O(2,K)
        H = CR_HYPOT(CR_HYPOT(REAL(G(Q,P)), AIMAG(G(Q,P))), CR_HYPOT(REAL(G(P,Q)), AIMAG(G(P,Q))))
        IF ((H .GT. ZERO) .OR. (AIMAG(G(P,P)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(P,P))) .NE. ONE) .OR. &
             (AIMAG(G(Q,Q)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(Q,Q))) .NE. ONE) .OR. &
             (REAL(G(P,P)) .LT. REAL(G(Q,Q)))) THEN
           CALL XENC(D(K), H, P, Q)
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
        H = CR_HYPOT(CR_HYPOT(REAL(G(Q,P)), AIMAG(G(Q,P))), CR_HYPOT(REAL(G(P,Q)), AIMAG(G(P,Q))))
        IF ((H .GT. ZERO) .OR. (AIMAG(G(P,P)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(P,P))) .NE. ONE) .OR. &
             (AIMAG(G(Q,Q)) .NE. ZERO) .OR. (SIGN(ONE,REAL(G(Q,Q))) .NE. ONE) .OR. &
             (REAL(G(P,P)) .LT. REAL(G(Q,Q)))) THEN
           CALL XENC(D(K), H, P, Q)
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
        P = 0; Q = 0
        CALL XDEC(W, P, Q)
        K = M + INFO
        O(1,K) = P
        O(2,K) = Q
        IF (INFO .GE. L) EXIT
        W = MONE
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              I = 0; J = 0
              CALL XDEC(D(K), I, J)
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
        P = 0; Q = 0
        CALL XDEC(W, P, Q)
        K = M + INFO
        O(1,K) = P
        O(2,K) = Q
        IF (INFO .GE. L) EXIT
        W = MONE
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W)
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              I = 0; J = 0
              CALL XDEC(D(K), I, J)
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
END SUBROUTINE WMKDPQ
