!>@brief \b XPQSORT sorts the arrays \f$AW,AP,AQ\f$ by the OpenMP-parallel merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the WPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE XPQSORT(WPQCMP, N, AW, AP, AQ, BW, BP, BQ, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=10), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=10), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: WPQCMP, XPQMRG
  PROCEDURE(), POINTER :: WPQMRG => XPQMRG
  INTEGER :: I, J, K, L, M, X, Y
  LOGICAL :: FLIP
  INCLUDE 'gpqsort.f90'
END SUBROUTINE XPQSORT
