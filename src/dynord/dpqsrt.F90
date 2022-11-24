!>@brief \b DPQSRT sorts the arrays \f$AW,AP,AQ\f$ by the sequential merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the DPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE DPQSRT(N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL64), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=REAL64), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: DPQCMP, DPQMRG
  ! PROCEDURE(), POINTER :: PQCMP => DPQCMP, PQMRG => DPQMRG
  INTEGER :: I, J, K, L, X, Y
  LOGICAL :: FLIP
#define PQCMP DPQCMP
#define PQMRG DPQMRG
#include "gpqsrt.F90"
END SUBROUTINE DPQSRT
