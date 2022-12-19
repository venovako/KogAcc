!>@brief \b SBRDG borders a single precision real square matrix of order \f$N\f$ to a matrix of order \f$M\ge N\f$.
!!
!!@param M [IN]; \f$M\f$ is the final matrix order.
!!@param N [IN]; \f$N\f$ is the initial matrix order (at most \f$M\f$).
!!@param G [OUT]; \f$G\f$ is a single precision real square matrix that gets bordered, such that the \f$M-N\f$ appended rows and columns are set to zero, while the first \f$N\f$ rows and columns of \f$G\f$ are not touched.
!!@param LDG [IN]; the leading dimension of \f$G\f$.
!!@param INFO [OUT]; zero on success or \f$-i\f$ if the \f$i\f$th argument had an illegal value.
PURE SUBROUTINE SBRDG(M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  REAL(KIND=REAL32), PARAMETER :: ZERO = 0.0_REAL32
  INTEGER, INTENT(IN) :: M, N, LDG
  REAL(KIND=REAL32), INTENT(OUT) :: G(LDG,M)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I, J
  INCLUDE 'gbrdg.f90'
END SUBROUTINE SBRDG
