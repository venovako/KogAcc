!>@brief \b XROTR premultiplies the rows \f$(p,q)\f$ of \f$G\f$ by \f$W\f$.
PURE SUBROUTINE XROTR(M, N, G, LDG, P, Q, W, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  REAL(KIND=10), INTENT(INOUT) :: G(LDG,N)
  REAL(KIND=10), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=10) :: X, Y
  INTEGER :: J
  INCLUDE 'grotr.f90'
END SUBROUTINE XROTR
