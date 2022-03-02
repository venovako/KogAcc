!>@brief \b QKSVD2X tests the QKSVD2 routine.
PROGRAM QKSVD2X
  IMPLICIT NONE
  EXTERNAL :: QKSVD2
  CALL TEST(QKSVD2)
CONTAINS
  SUBROUTINE TEST(KSVD2)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
    IMPLICIT NONE
    INTEGER, PARAMETER :: IERR = -HUGE(0)
    EXTERNAL :: KSVD2
    REAL(KIND=REAL128) :: G(2,2), U(2,2), V(2,2), S(2)
    INTEGER :: INFO
    INCLUDE 'tksvd2.f90'
1   FORMAT(A,ES45.36E4)
2   FORMAT(A,I6)
  END SUBROUTINE TEST
END PROGRAM QKSVD2X
