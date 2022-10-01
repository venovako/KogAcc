!>@brief \b XPQCMP compares two \f$(w,p,q)\f$ tuples, \f$x\f$ and \f$y\f$, with \f$w\f$ in extended precision, and returns INFO that is less than, equal to, or greater than zero if \f$x\f$ should preceed, is equal to, or should come after \f$y\f$, respectively.
PURE SUBROUTINE XPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
  REAL(KIND=10), INTENT(IN) :: XW, YW
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: XB, YB
  INCLUDE 'gpqcmp.f90'
END SUBROUTINE XPQCMP
