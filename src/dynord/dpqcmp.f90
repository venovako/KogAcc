!>@brief \b DPQCMP compares two (w,p,q) tuples, x and y, with w in double precision, and returns INFO that is less than, equal to, or greater than zero if x should preceed, is equal to, or should come after y, respectively.
PURE SUBROUTINE DPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
  REAL(KIND=REAL64), INTENT(IN) :: XW, YW
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: XB, YB
  INCLUDE 'gpqcmp.f90'
END SUBROUTINE DPQCMP
