!>@brief \b ZKERR2 computes the errors in the SVD of G.
PURE SUBROUTINE ZKERR2(G, U, V, S, E, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL128
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  COMPLEX(KIND=K), PARAMETER :: CONE = (ONE,ZERO) !, CZERO = (ZERO,ZERO)
  COMPLEX(KIND=REAL64), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
  REAL(KIND=REAL64), INTENT(IN) :: S(2)
  REAL(KIND=K), INTENT(OUT) :: E(3)
  INTEGER, INTENT(INOUT) :: INFO
  COMPLEX(KIND=K) :: GX(2,2), UX(2,2), VX(2,2), SX(2,2)
  INCLUDE 'hkerr2.f90'
END SUBROUTINE ZKERR2
