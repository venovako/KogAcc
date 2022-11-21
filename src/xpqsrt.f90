!>@brief \b XPQSRT sorts the arrays \f$AW,AP,AQ\f$ by the sequential merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the XPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE XPQSRT(N, AW, AP, AQ, BW, BP, BQ, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=10), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=10), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: XPQCMP, XPQMRG
  PROCEDURE(), POINTER :: WPQCMP => XPQCMP, WPQMRG => XPQMRG
  INTEGER :: I, J, K, L, X, Y
  LOGICAL :: FLIP
  INCLUDE 'gpqsrt.f90'
END SUBROUTINE XPQSRT
