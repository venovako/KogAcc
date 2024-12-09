!>@brief \b XLWSV2X tests the XLWSV2 routine.
PROGRAM XLWSV2X
  IMPLICIT NONE
  CALL XLTEST
CONTAINS
  !>@brief \b XLTEST calls XLWSV2.
  SUBROUTINE XLTEST
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INPUT_UNIT, REAL128
    IMPLICIT NONE
    INTERFACE
       SUBROUTINE XLWSV2(G, U, V, S, INFO)
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
         IMPLICIT NONE
         REAL(KIND=c_long_double), INTENT(IN) :: G(2,2)
         REAL(KIND=c_long_double), INTENT(OUT) :: U(2,2), V(2,2), S(2)
         INTEGER, INTENT(OUT) :: INFO
       END SUBROUTINE XLWSV2
    END INTERFACE
    INTERFACE
       SUBROUTINE XKERR2(G, U, V, S, E, INFO)
         USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
         USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
         IMPLICIT NONE
         REAL(KIND=c_long_double), INTENT(IN) :: G(2,2), U(2,2), V(2,2), S(2)
         REAL(KIND=REAL128), INTENT(OUT) :: E(3)
         INTEGER, INTENT(IN) :: INFO(3)
       END SUBROUTINE XKERR2
    END INTERFACE
    INTEGER, PARAMETER :: KX = REAL128, IERR = -HUGE(0)-1, CLAL = 64
    CHARACTER(LEN=CLAL) :: CLA
    REAL(KIND=KX) :: E(3)
    REAL(KIND=c_long_double) :: G(2,2), U(2,2), V(2,2), S(2)
    INTEGER :: I, INFO, INFO3(3)
    LOGICAL :: ONCE
    G(2,1) = 0.0_c_long_double
#define LSVD2 XLWSV2
#define KERR2 XKERR2
#include "tlasv2.F90"
1   FORMAT(A,ES30.21E4)
2   FORMAT(A,I6)
  END SUBROUTINE XLTEST
END PROGRAM XLWSV2X
