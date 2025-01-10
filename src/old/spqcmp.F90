!>@brief \b SPQCMP compares two (w,p,q) tuples, x and y, with w in single precision, and returns INFO that is less than, equal to, or greater than zero if x should preceed, is equal to, or should come after y, respectively.
PURE SUBROUTINE SPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
  REAL(KIND=REAL32), INTENT(IN) :: XW, YW
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: XB, YB
#include "gpqcmp.F90"
END SUBROUTINE SPQCMP
