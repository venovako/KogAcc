!>@brief \b IBDIMS computes various dimensions based on N and B.
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
  INTEGER, INTENT(OUT) :: M, M_B, NW, ND, NO
  INTEGER, INTENT(INOUT) :: N, B, INFO

  INTEGER :: T, M_P

  CALL NB2M(N, B, M, M_B)
  IF (M_B .NE. 0) THEN
     INFO = M_B
     RETURN
  END IF
  M_B = M / B
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
  NW = MAX(ND, 3 * NO * M_P)
  IF (MOD(M_B, 2) .EQ. 0) THEN
     NO = M_P * (M_B - 1)
  ELSE ! M_B odd
     NO = M_B * ((M_B - 1) / 2)
  END IF
  ND = M_P * B * (2 * B - 1)
  ND = MAX(ND, NO + 1)
  NO = NO + M_P + 2 * B * B
  B = T
END SUBROUTINE IBDIMS
