!>@brief \b YKSVD2X tests the YKSVD2 routine.
PROGRAM YKSVD2X
  IMPLICIT NONE
  CALL YTEST
CONTAINS
  !>@brief \b YTEST calls YKSVD2.
  SUBROUTINE YTEST
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INPUT_UNIT, REAL128
    IMPLICIT NONE
#define CR_HYPOT HYPOT
    INTERFACE
       SUBROUTINE YKSVD2(G, U, V, S, INFO)
         USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
         IMPLICIT NONE
         COMPLEX(KIND=REAL128), INTENT(IN) :: G(2,2)
         COMPLEX(KIND=REAL128), INTENT(OUT) :: U(2,2), V(2,2)
         REAL(KIND=REAL128), INTENT(OUT) :: S(2)
         INTEGER, INTENT(INOUT) :: INFO(3)
       END SUBROUTINE YKSVD2
    END INTERFACE
    INTEGER, PARAMETER :: KX = REAL128, IERR = -HUGE(0)-1, CLAL = 132
    CHARACTER(LEN=CLAL) :: CLA
    COMPLEX(KIND=REAL128) :: G(2,2), U(2,2), V(2,2)
    REAL(KIND=REAL128) :: S(2)
    INTEGER :: I, J, INFO(3)
    LOGICAL :: ONCE
    COMPLEX(KIND=KX) :: GX(2,2), UX(2,2), VX(2,2)
    REAL(KIND=KX) :: SX(2,2)
#define KSVD2 YKSVD2
#include "uksvd2.F90"
1   FORMAT(A,ES45.36E4)
2   FORMAT(A,'(',ES45.36E4,',',ES45.36E4,')')
3   FORMAT(A,I6)
  END SUBROUTINE YTEST
END PROGRAM YKSVD2X
