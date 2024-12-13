!>@brief \b DROTRX premultiplies the rows (p,q) of G by W using an (imperfect) emulation of an accurate a*b+c*d operation.
SUBROUTINE DROTRX(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64, L = c_long_double
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
#define VL 8
  REAL(KIND=K) :: X(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: X
  REAL(KIND=K) :: Y(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: Y
  REAL(KIND=L) :: XX(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: XX
  REAL(KIND=L) :: YY(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: YY
  REAL(KIND=L) :: WW(2,2)
  !DIR$ ATTRIBUTES ALIGN: 64:: WW
  INTEGER :: I, J
#define HL 4
#include "grotra.F90"
END SUBROUTINE DROTRX
