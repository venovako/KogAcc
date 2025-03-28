!>@brief CRND2T tests a set of pseudorandom single precision complex 2x2 matrices.
PROGRAM CRND2T
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128, OUTPUT_UNIT, ERROR_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE CKSVD2(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       COMPLEX(KIND=REAL32), INTENT(IN) :: G(2,2)
       COMPLEX(KIND=REAL32), INTENT(OUT) :: U(2,2), V(2,2)
       REAL(KIND=REAL32), INTENT(OUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE CKSVD2
  END INTERFACE
  INTERFACE
     SUBROUTINE CKERR2(G, U, V, S, E, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL128
       IMPLICIT NONE
       COMPLEX(KIND=REAL32), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
       REAL(KIND=REAL32), INTENT(IN) :: S(2)
       REAL(KIND=REAL128), INTENT(OUT) :: E(3)
       INTEGER, INTENT(IN) :: INFO(3)
     END SUBROUTINE CKERR2
  END INTERFACE
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
  INTERFACE
     SUBROUTINE STHALT(MSG)
       IMPLICIT NONE
       CHARACTER(LEN=*), INTENT(IN) :: MSG
     END SUBROUTINE STHALT
  END INTERFACE

  INTEGER, PARAMETER :: D = REAL32
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
  COMPLEX(KIND=D) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=D) :: S(2,5)
  COMPLEX(KIND=REAL128) :: QG(2,2), QU(2,2), QV(2,2)
  REAL(KIND=REAL128) :: QS(2), E(5), F(2,6), Q
  INTEGER :: SSIZE, I, J, K, L, M, N, INFO(3)
  CHARACTER(LEN=64) :: CLA
  INTEGER, ALLOCATABLE :: ISEED(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: ISEED
#define KSVD2 CKSVD2
#define KERR2 CKERR2
#include "hrnd2t.F90"
1 FORMAT(A,ES17.9E3)
END PROGRAM CRND2T
