!>@brief \b QROTC postmultiplies the columns (p,q) of G by W.
PURE SUBROUTINE QROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=REAL128), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL128) :: X, Y
  INTEGER :: I
  INCLUDE 'grotc.f90'
END SUBROUTINE QROTC
