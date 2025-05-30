!>@brief \b SLWSV2X tests the SLWSV2 routine.
PROGRAM SLWSV2X
  IMPLICIT NONE
  CALL SLTEST
CONTAINS
  !>@brief \b SLTEST calls SLWSV2.
  SUBROUTINE SLTEST
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INPUT_UNIT, REAL32, REAL128
    IMPLICIT NONE
    INTERFACE
       SUBROUTINE SLWSV2(G, U, V, S, INFO)
         USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
         IMPLICIT NONE
         REAL(KIND=REAL32), INTENT(IN) :: G(2,2)
         REAL(KIND=REAL32), INTENT(OUT) :: U(2,2), V(2,2), S(2)
         INTEGER, INTENT(OUT) :: INFO
       END SUBROUTINE SLWSV2
    END INTERFACE
    INTERFACE
       SUBROUTINE SKERR2(G, U, V, S, E, INFO)
         USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128
         IMPLICIT NONE
         REAL(KIND=REAL32), INTENT(IN) :: G(2,2), U(2,2), V(2,2), S(2)
         REAL(KIND=REAL128), INTENT(OUT) :: E(3)
         INTEGER, INTENT(IN) :: INFO(3)
       END SUBROUTINE SKERR2
    END INTERFACE

    INTEGER, PARAMETER :: KX = REAL128, IERR = -HUGE(0), CLAL = 64
    CHARACTER(LEN=CLAL) :: CLA
    REAL(KIND=KX) :: E(3)
    REAL(KIND=REAL32) :: G(2,2), U(2,2), V(2,2), S(2)
    INTEGER :: I, INFO, INFO3(3)
    LOGICAL :: ONCE
#define LSVD2 SLWSV2
#define KERR2 SKERR2
    G(2,1) = 0.0_REAL32
#include "tlasv2.F90"
1   FORMAT(A,ES16.9E2)
2   FORMAT(A,I4)
  END SUBROUTINE SLTEST
END PROGRAM SLWSV2X
