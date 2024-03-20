!>@brief \b XPQCMP compares two (w,p,q) tuples, x and y, with w in extended precision, and returns INFO that is less than, equal to, or greater than zero if x should preceed, is equal to, or should come after y, respectively.
PURE SUBROUTINE XPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
  REAL(KIND=10), INTENT(IN) :: XW, YW
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: XB, YB
#include "gpqcmp.F90"
END SUBROUTINE XPQCMP
