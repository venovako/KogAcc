!>@brief \b SROTC postmultiplies the columns \f$(p,q)\f$ of \f$G\f$ by \f$W\f$.
PURE SUBROUTINE SROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=REAL32), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=REAL32), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL32) :: X, Y
  INTEGER :: I
  INCLUDE 'grotc.f90'
END SUBROUTINE SROTC