!>@brief \b QPQSORT sorts the arrays \f$AW,AP,AQ\f$ by the OpenMP-parallel merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the QPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE QPQSORT(N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL128), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=REAL128), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: QPQCMP, QPQMRG
  INTEGER :: I, J, K, L, M, X, Y
  LOGICAL :: FLIP
#define PQCMP QPQCMP
#define PQMRG QPQMRG
#include "gpqsort.F90"
END SUBROUTINE QPQSORT
