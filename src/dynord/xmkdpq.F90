!>@brief \b XMKDPQ builds at most N/2 pivot index pairs for the next transformation of G.
SUBROUTINE XMKDPQ(N, M, D, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE PVN_DJS_XDEC(E, P, Q)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN) :: E
       INTEGER, INTENT(OUT) :: P, Q
     END SUBROUTINE PVN_DJS_XDEC
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
  IF (L .EQ. -1) THEN
     L = 0
     S = M
     DO INFO = 1, R
        CALL PVN_DJS_XDEC(W, P, Q)
        O(1,INFO) = P
        O(2,INFO) = Q
        IF (INFO .GE. R) EXIT
        W = WZERO
        K = 1
        DO WHILE (K .LE. S)
           IF (D(K) .GT. WZERO) THEN
              CALL PVN_DJS_XDEC(D(K), I, J)
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
  ELSE IF (L .EQ. 0) THEN
     S = M
     DO INFO = 1, R
        CALL XDECAP(W, P, Q)
        O(1,INFO) = P
        O(2,INFO) = Q
        IF (INFO .GE. R) EXIT
        W = WZERO
        K = 1
        DO WHILE (K .LE. S)
           IF (D(K) .GT. WZERO) THEN
              CALL XDECAP(D(K), I, J)
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
  ELSE IF (L .GT. 0) THEN
     DO INFO = 1, R
        CALL XDECAP(W, P, Q)
        O(1,INFO) = P
        O(2,INFO) = Q
        IF (INFO .GE. R) EXIT
        W = WZERO
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W)
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              CALL XDECAP(D(K), I, J)
              IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
                 W = MAX(W, D(K))
              ELSE ! colliding
                 D(K) = -D(K)
              END IF
           END IF
        END DO
        !$OMP END PARALLEL DO
        IF (W .LE. WZERO) EXIT
     END DO
  ELSE ! L .LT. -1
     L = L + 1
     DO INFO = 1, R
        CALL PVN_DJS_XDEC(W, P, Q)
        O(1,INFO) = P
        O(2,INFO) = Q
        IF (INFO .GE. R) EXIT
        W = WZERO
        !$OMP PARALLEL DO DEFAULT(NONE) SHARED(D,M,P,Q) PRIVATE(I,J,K) REDUCTION(MAX:W)
        DO K = 1, M
           IF (D(K) .GT. WZERO) THEN
              CALL PVN_DJS_XDEC(D(K), I, J)
              IF ((I .NE. P) .AND. (I .NE. Q) .AND. (J .NE. P) .AND. (J .NE. Q)) THEN
                 W = MAX(W, D(K))
              ELSE ! colliding
                 D(K) = -D(K)
              END IF
           END IF
        END DO
        !$OMP END PARALLEL DO
        IF (W .LE. WZERO) EXIT
     END DO
  END IF
CONTAINS
  PURE SUBROUTINE XDECAP(H, P, Q)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#ifdef HAVE_UNSIGNED
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: UINT8
#else
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
#endif
    IMPLICIT NONE
    REAL(KIND=c_long_double), INTENT(IN) :: H
    INTEGER, INTENT(OUT) :: P, Q
    REAL(KIND=c_long_double) :: W
#ifdef HAVE_UNSIGNED
    UNSIGNED(KIND=UNIT8) :: B(16)
#else
    INTEGER(KIND=INT8) :: B(16)
#endif
    EQUIVALENCE(W, B)
    W = H
#ifdef HAVE_UNSIGNED
    Q = INT(B(1)) + 1
    P = INT(B(2)) + 1
#else
    Q = B(1) + 1
    P = B(2) + 1
#endif
  END SUBROUTINE XDECAP
END SUBROUTINE XMKDPQ
