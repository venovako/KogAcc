!>@brief \b ZKSVD2X tests the ZKSVD2 routine.
PROGRAM ZKSVD2X
  IMPLICIT NONE
  EXTERNAL :: ZKSVD2
  CALL ZTEST(ZKSVD2)
CONTAINS
  !>@brief \b ZTEST calls KSVD2.
  !!
  !!@param KSVD2 is ZKSVD2.
  SUBROUTINE ZTEST(KSVD2)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
    IMPLICIT NONE
    INTEGER, PARAMETER :: IERR = -HUGE(0), KX = MAX(REAL64, REAL128), CLAL = 132
    EXTERNAL :: KSVD2
    CHARACTER(LEN=CLAL) :: CLA
    COMPLEX(KIND=REAL64) :: G(2,2), U(2,2), V(2,2)
    REAL(KIND=REAL64) :: S(2)
    INTEGER :: INFO
    LOGICAL :: ONCE
    COMPLEX(KIND=KX) :: GX(2,2), UX(2,2), VX(2,2)
    REAL(KIND=KX) :: SX(2,2)
    INCLUDE 'uksvd2.f90'
1   FORMAT(A,ES25.17E3)
2   FORMAT(A,'(',ES25.17E3,',',ES25.17E3,')')
3   FORMAT(A,I5)
  END SUBROUTINE ZTEST
END PROGRAM ZKSVD2X
