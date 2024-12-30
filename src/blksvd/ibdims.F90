!>@brief \b IBDIMS computes various dimensions based on N, B, and M = JS.
PURE SUBROUTINE IBDIMS(N, B, M, M_B, NW, ND, NO, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE NB2M(N, B, M, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, B
       INTEGER, INTENT(OUT) :: M, INFO
     END SUBROUTINE NB2M
  END INTERFACE
#ifndef CLS
#define CLS 64
#endif
  INTEGER, INTENT(INOUT) :: N, B, M, INFO
  INTEGER, INTENT(OUT) :: M_B, NW, ND, NO

  INTEGER :: J, T, M_P, B_P

  SELECT CASE (M)
  CASE (0,1,2,3,4)
     J = M
  CASE DEFAULT
     INFO = -3
     RETURN
  END SELECT
  CALL NB2M(N, B, M, M_B)
  IF (M_B .NE. 0) THEN
     INFO = M_B
     RETURN
  END IF
  M_B = M / B
  IF (((J .EQ. 2) .OR. (J .EQ. 4)) .AND. (MOD(M_B, 2) .NE. 0)) THEN
     M = M + B
     M_B = M_B + 1
  END IF
  IF (INFO .GT. 0) THEN
     NW = 3
  ELSE ! INFO .LE. 0
     NW = 5
  END IF
  SELECT CASE (INFO)
  CASE (REAL32)
     T = CLS / 4
  CASE (-REAL32, REAL64)
     T = CLS / 8
  CASE (-REAL64, REAL128)
     T = CLS / 16
  CASE (-REAL128)
     T = CLS / 32
  CASE DEFAULT
     T = -8
  END SELECT
  INFO = T
  IF (INFO .LT. 0) RETURN

  T = MOD(M, INFO)
  IF (T .NE. 0) THEN
     N = M + (INFO - T)
  ELSE ! MOD(M, INFO) .EQ. 0
     N = M
  END IF
  ND = 2 * B
  T = MOD(ND, INFO)
  IF (T .NE. 0) THEN
     T = ND + (INFO - T)
  ELSE ! MOD(2*B, INFO) .EQ. 0
     T = ND
  END IF
  ND = M_B * M_B
  NO = T * 2 * B
  M_P = M_B / 2
  ! LAYOUT OF W:
  ! M_B x M_B
  ! (3 * (LDB x 2*B) * M_P); ((MAX((2*B-1),NW) * 2*B) * M_P)
  IF (NW .EQ. 3) THEN
     NW = MAX(6, MAX(ND, ((3 * NO + MAX((2 * B - 1), NW) * 2 * B) * M_P)))
  ELSE ! W complex
     NW = MAX(3, MAX(((ND + MOD(ND, 2)) / 2), ((3 * NO + MAX((2 * B - 1), NW) * B) * M_P)))
  END IF
  IF (J .EQ. 4) THEN
     NO = M_P * M_B
  ELSE ! J .NE. 4
     IF (MOD(M_B, 2) .EQ. 0) THEN
        NO = M_P * (M_B - 1)
     ELSE ! M_B odd
        NO = M_B * ((M_B - 1) / 2)
     END IF
  END IF
  B_P = B * (2 * B - 1)
  ! LAYOUT OF D: D(X) = (X*(X-1))/2 + 1
  ! D(M_B) [ if J .EQ. 3 ]
  ! D(2*B)_1 ... D(2*B)_M_P
  IF (J .EQ. 3) THEN
     ND = MAX((NO + 1), (B_P + 1) * M_P)
  ELSE ! J .NE. 3
     ND = (B_P + 1) * M_P
  END IF
  ! LAYOUT OF O: RC(X) = (X*(X-1))/2
  ! RCJ(M_B) RC(2*B) OD
  ! LAYOUT OF OD: PQI(X) = X
  ! PQI(M_B) PQI(2*B)_1 ... PQI(2*B)_M_P
  NO = NO + B_P + M_B + 2 * B * M_P
  B = T
END SUBROUTINE IBDIMS
