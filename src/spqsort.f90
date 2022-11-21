!>@brief \b SPQSORT sorts the arrays \f$AW,AP,AQ\f$ by the OpenMP-parallel merge sort algorithm, using the workspace arrays \f$BW,BP,BQ\f$ of the same length \f$N\f$, according to the ordering \f$\prec\f$ defined by the SPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE SPQSORT(N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  REAL(KIND=REAL32), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N)
  REAL(KIND=REAL32), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N), INFO
  EXTERNAL :: SPQCMP, SPQMRG
  PROCEDURE(), POINTER :: WPQCMP => SPQCMP, WPQMRG => SPQMRG
  INTEGER :: I, J, K, L, M, X, Y
  LOGICAL :: FLIP
  INCLUDE 'gpqsort.f90'
END SUBROUTINE SPQSORT
