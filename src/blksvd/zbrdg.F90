!>@brief \b ZBRDG borders a double precision complex square matrix of order N to a matrix of order M >= N.
!!
!!@param M [IN]; M is the final matrix order.
!!@param N [IN]; N is the initial matrix order (at most M).
!!@param G [OUT]; G is a double precision complex square matrix that gets bordered, such that the M-N appended rows and columns are set to zero, while the first N rows and columns of G are not touched.
!!@param LDG [IN]; the leading dimension of G.
!!@param INFO [INOUT]; on input, non-zero for parallel execution; on output, zero on success or -i if the i-th argument had an illegal value.
SUBROUTINE ZBRDG(M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL64
  COMPLEX(KIND=K), PARAMETER :: ZERO = (0.0_K,0.0_K)
  INTEGER, INTENT(IN) :: M, N, LDG
  COMPLEX(KIND=K), INTENT(OUT) :: G(LDG,M)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J
#include "gbrdg.F90"
END SUBROUTINE ZBRDG
