!>@brief \b SPQSRT sorts the arrays \f$AW,AP,AQ\f$ by the sequential merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the SPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE SPQSRT(N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL32), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=REAL32), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: SPQCMP, SPQMRG
  ! PROCEDURE(), POINTER :: PQCMP => SPQCMP, PQMRG => SPQMRG
  INTEGER :: I, J, K, L, X, Y
  LOGICAL :: FLIP
#define PQCMP SPQCMP
#define PQMRG SPQMRG
#include "gpqsrt.F90"
END SUBROUTINE SPQSRT
