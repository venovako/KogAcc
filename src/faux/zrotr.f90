!>@brief \b ZROTR premultiplies the rows \f$(p,q)\f$ of \f$G\f$ by \f$W\f$.
PURE SUBROUTINE ZROTR(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=REAL64), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(OUT) :: INFO
  COMPLEX(KIND=REAL64) :: X, Y
  INTEGER :: J
  INCLUDE 'grotr.f90'
END SUBROUTINE ZROTR
