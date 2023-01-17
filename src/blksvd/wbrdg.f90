!>@brief \b WBRDG borders an extended precision complex square matrix of order N to a matrix of order M >= N.
!!
!!@param M [IN]; M is the final matrix order.
!!@param N [IN]; N is the initial matrix order (at most M).
!!@param G [OUT]; G is an extended precision complex square matrix that gets bordered, such that the M-N appended rows and columns are set to zero, while the first N rows and columns of G are not touched.
!!@param LDG [IN]; the leading dimension of G.
!!@param INFO [INOUT]; on input, non-zero for parallel execution; on output, zero on success or -i if the i-th argument had an illegal value.
SUBROUTINE WBRDG(M, N, G, LDG, INFO)
  IMPLICIT NONE
  COMPLEX(KIND=10), PARAMETER :: ZERO = (0.0_10,0.0_10)
  INTEGER, INTENT(IN) :: M, N, LDG
  COMPLEX(KIND=10), INTENT(OUT) :: G(LDG,M)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J
  INCLUDE 'gbrdg.f90'
END SUBROUTINE WBRDG
