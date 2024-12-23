!>@brief \b XMKDPQ builds at most N/2 pivot index pairs for the next transformation of G.
SUBROUTINE XMKDPQ(N, M, D, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE XDEC(E, P, Q) BIND(C,NAME='pvn_djs_xdec_')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN) :: E
       INTEGER, INTENT(OUT) :: P, Q
     END SUBROUTINE XDEC
  END INTERFACE

  REAL(KIND=c_long_double), PARAMETER :: WZERO = 0.0_c_long_double
  INTEGER, INTENT(IN) :: N, M
  REAL(KIND=c_long_double), INTENT(INOUT) :: D(M+1)
  INTEGER, INTENT(OUT) :: O(2,N/2)
  INTEGER, INTENT(INOUT) :: INFO

  REAL(KIND=c_long_double) :: W
  INTEGER :: I, J, K, L, P, Q, R, S

  ! find the remaining pivots
  L = INFO
  INFO = 0
  IF (N .LT. 0) INFO = -1
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (M .EQ. 0) RETURN
  W = D(M+1)
  IF (W .LE. WZERO) RETURN

  R = N / 2
  S = M
  DO INFO = 1, R
     P = 0; Q = 0
     CALL XDEC(W, P, Q)
     O(1,INFO) = P
     O(2,INFO) = Q
     IF (INFO .GE. R) EXIT
     W = WZERO
     K = 1
     DO WHILE (K .LE. S)
        IF (D(K) .GT. WZERO) THEN
           I = 0; J = 0
           CALL XDEC(D(K), I, J)
           IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
              W = MAX(W, D(K))
           ELSE ! colliding
              D(K) = D(S)
              K = K - 1
              S = S - 1
           END IF
        END IF
        K = K + 1
     END DO
     IF (W .LE. WZERO) EXIT
  END DO
END SUBROUTINE XMKDPQ
