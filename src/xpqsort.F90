!>@brief \b XPQSORT sorts the arrays \f$AW,AP,AQ\f$ by the OpenMP-parallel merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the XPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE XPQSORT(N, AW, AP, AQ, BW, BP, BQ, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=10), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=10), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: XPQCMP, XPQMRG
  INTEGER :: I, J, K, L, M, X, Y
  LOGICAL :: FLIP
#define PQCMP XPQCMP
#define PQMRG XPQMRG
#include "gpqsort.F90"
END SUBROUTINE XPQSORT
