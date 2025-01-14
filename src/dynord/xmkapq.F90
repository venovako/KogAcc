!>@brief \b XMKAPQ builds at most N/2 pivot index pairs for the next transformation of G.
PURE SUBROUTINE XMKAPQ(N, M, D, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
  IMPLICIT NONE

  REAL(KIND=c_long_double), PARAMETER :: WZERO = 0.0_c_long_double
  INTEGER, INTENT(IN) :: N, M
  REAL(KIND=c_long_double), INTENT(INOUT) :: D(M+1)
  INTEGER, INTENT(OUT) :: O(2,N/2)
  INTEGER, INTENT(INOUT) :: INFO

  REAL(KIND=c_long_double) :: W, H
  INTEGER(KIND=INT8) :: B(16)
  EQUIVALENCE(H, B)
  INTEGER :: I, J, K, L, P, Q, R, S

  ! find the remaining pivots
  L = INFO
  INFO = 0
#ifndef NDEBUG
  IF (N .LT. 0) INFO = -1
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
#endif
  IF (N .EQ. 0) RETURN
  IF (M .EQ. 0) RETURN
  W = D(M+1)
  IF (W .LE. WZERO) RETURN

  R = N / 2
  S = M
  DO INFO = 1, R
     H = W
     Q = B(1)
     P = B(2)
     O(1,INFO) = P
     O(2,INFO) = Q
     IF (INFO .GE. R) EXIT
     W = WZERO
     K = 1
     DO WHILE (K .LE. S)
        IF (D(K) .GT. WZERO) THEN
           H = D(K)
           J = B(1)
           I = B(2)
           IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
              W = MAX(W, D(K))
              K = K + 1
           ELSE ! colliding
              D(K) = D(S)
              S = S - 1
           END IF
        ELSE ! can be skipped
           D(K) = D(S)
           S = S - 1
        END IF
     END DO
     IF (W .LE. WZERO) EXIT
  END DO
END SUBROUTINE XMKAPQ
