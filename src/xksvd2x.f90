!>@brief \b XKSVD2X tests the XKSVD2 routine.
PROGRAM XKSVD2X
  IMPLICIT NONE
  EXTERNAL :: XKSVD2
  CALL XTEST(XKSVD2)
CONTAINS
  !>@brief \b XTEST calls KSVD2.
  !!
  !!@param KSVD2 is XKSVD2.
  SUBROUTINE XTEST(KSVD2)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
    IMPLICIT NONE
    INTEGER, PARAMETER :: IERR = -HUGE(0), KX = MAX(10, REAL128), CLAL = 64
    EXTERNAL :: KSVD2
    CHARACTER(LEN=CLAL) :: CLA
    REAL(KIND=10) :: G(2,2), U(2,2), V(2,2), S(2)
    INTEGER :: INFO
    REAL(KIND=KX) :: GX(2,2), UX(2,2), VX(2,2), SX(2,2)
    INCLUDE 'tksvd2.f90'
1   FORMAT(A,ES30.21E4)
2   FORMAT(A,I6)
  END SUBROUTINE XTEST
END PROGRAM XKSVD2X
