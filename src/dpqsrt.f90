!>@brief \b DPQSRT sorts the arrays \f$AW,AP,AQ\f$ by the sequential merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the WPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE DPQSRT(WPQCMP, N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL64), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=REAL64), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: WPQCMP, DPQMRG
  PROCEDURE(), POINTER :: WPQMRG => DPQMRG
  INTEGER :: I, J, K, L, X, Y
  LOGICAL :: FLIP
  INCLUDE 'gpqsrt.f90'
END SUBROUTINE DPQSRT
