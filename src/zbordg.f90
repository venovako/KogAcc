!>@brief \b ZBORDG borders a double precision complex square matrix of order \f$N\f$ to a matrix of order \f$M\ge N\f$.
!!
!!@param M [IN]; \f$M\f$ is the final matrix order.
!!@param N [IN]; \f$N\f$ is the initial matrix order (at most \f$M\f$).
!!@param G [OUT]; \f$G\f$ is a double precision complex square matrix that gets bordered, such that the \f$M-N\f$ appended rows and columns are set to zero, except on the diagonal, which is set to unity, while the first \f$N\f$ rows and columns of \f$G\f$ are not touched.
!!@param LDG [IN]; the leading dimension of \f$G\f$.
!!@param INFO [OUT]; zero on success or \f$-i\f$ if the \f$i\f$th argument had an illegal value.
SUBROUTINE ZBORDG(M, N, G, LDG, INFO)
  IMPLICIT NONE
  DOUBLE PRECISION, PARAMETER :: DZERO = 0.0D0, DONE = 1.0D0
  DOUBLE COMPLEX, PARAMETER :: ZERO = (DZERO,DZERO), ONE = (DONE,DZERO)
  INTEGER, INTENT(IN) :: M, N, LDG
  DOUBLE COMPLEX, INTENT(OUT) :: G(LDG,M)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J
  INCLUDE 'gbordg.f90'
END SUBROUTINE ZBORDG
