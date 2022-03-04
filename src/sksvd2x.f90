!>@brief \b SKSVD2X tests the SKSVD2 routine.
PROGRAM SKSVD2X
  IMPLICIT NONE
  EXTERNAL :: SKSVD2
  CALL TEST(SKSVD2)
CONTAINS
  SUBROUTINE TEST(KSVD2)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
    IMPLICIT NONE
    INTEGER, PARAMETER :: IERR = -HUGE(0), KX = MAX(REAL64, REAL128), CLAL = 64
    EXTERNAL :: KSVD2
    CHARACTER(LEN=CLAL) :: CLA
    REAL :: G(2,2), U(2,2), V(2,2), S(2)
    INTEGER :: INFO
    REAL(KIND=KX) :: GX(2,2), UX(2,2), VX(2,2), SX(2,2)
    INCLUDE 'tksvd2.f90'
1   FORMAT(A,ES16.9E2)
2   FORMAT(A,I4)
  END SUBROUTINE TEST
END PROGRAM SKSVD2X
