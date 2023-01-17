!>@brief \b YBRDG borders a quadruple precision complex square matrix of order N to a matrix of order M >= N.
!!
!!@param M [IN]; M is the final matrix order.
!!@param N [IN]; N is the initial matrix order (at most M).
!!@param G [OUT]; G is a quadruple precision complex square matrix that gets bordered, such that the M-N appended rows and columns are set to zero, while the first N rows and columns of G are not touched.
!!@param LDG [IN]; the leading dimension of G.
!!@param INFO [INOUT]; on input, non-zero for parallel execution; on output, zero on success or -i if the i-th argument had an illegal value.
SUBROUTINE YBRDG(M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  COMPLEX(KIND=REAL128), PARAMETER :: ZERO = (0.0_REAL128,0.0_REAL128)
  INTEGER, INTENT(IN) :: M, N, LDG
  COMPLEX(KIND=REAL128), INTENT(OUT) :: G(LDG,M)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J
  INCLUDE 'gbrdg.f90'
END SUBROUTINE YBRDG
