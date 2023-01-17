!>@brief \b QPQCMP compares two (w,p,q) tuples, x and y, with w in quadruple precision, and returns INFO that is less than, equal to, or greater than zero if x should preceed, is equal to, or should come after y, respectively.
PURE SUBROUTINE QPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
  REAL(KIND=REAL128), INTENT(IN) :: XW, YW
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: XB, YB
  INCLUDE 'gpqcmp.f90'
END SUBROUTINE QPQCMP
