!>@brief QRND2T tests a set of pseudorandom quadruple precision real 2x2 matrices.
PROGRAM QRND2T
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128, OUTPUT_UNIT, ERROR_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE QKSVD2(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       REAL(KIND=REAL128), INTENT(IN) :: G(2,2)
       REAL(KIND=REAL128), INTENT(OUT) :: U(2,2), V(2,2), S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE QKSVD2
  END INTERFACE
#define KSVD2 QKSVD2
#ifdef UPPER
  INTERFACE
     SUBROUTINE QLWSV2(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       REAL(KIND=REAL128), INTENT(IN) :: G(2,2)
       REAL(KIND=REAL128), INTENT(OUT) :: U(2,2), V(2,2), S(2)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE QLWSV2
  END INTERFACE
#define LWSV2 QLWSV2
#endif
  INTERFACE
     PURE SUBROUTINE QKERR2(G, U, V, S, E, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       REAL(KIND=REAL128), INTENT(IN) :: G(2,2), U(2,2), V(2,2), S(2)
       REAL(KIND=REAL128), INTENT(OUT) :: E(3)
       INTEGER, INTENT(IN) :: INFO(3)
     END SUBROUTINE QKERR2
  END INTERFACE
#define KERR2 QKERR2
  INTERFACE
     SUBROUTINE STHALT(MSG)
       IMPLICIT NONE
       CHARACTER(LEN=*), INTENT(IN) :: MSG
     END SUBROUTINE STHALT
  END INTERFACE
  INTEGER, PARAMETER :: D = REAL128
  REAL(KIND=REAL128), PARAMETER :: QZERO = 0.0_REAL128, QONE = 1.0_REAL128
#ifdef UPPER
  REAL(KIND=D), PARAMETER :: ZERO = 0.0_D
#endif
  INTEGER, ALLOCATABLE :: ISEED(:)
  REAL(KIND=D) :: G(2,2), U(2,2), V(2,2), S(2), T
  REAL(KIND=REAL128) :: QG(2,2), QU(2,2), QV(2,2), QS(2), Q
#ifdef UPPER
  REAL(KIND=REAL128) :: E(5,2), F(2,16)
#else
  REAL(KIND=REAL128) :: E(5,1), F(2,6)
#endif
  INTEGER :: SSIZE, I, J, K, L, M, N, INFO(3)
  CHARACTER(LEN=64) :: CLA
#include "grnd2t.F90"
1 FORMAT(A,ES45.36E4)
END PROGRAM QRND2T
