!>@brief \b QXOFFSQ computes, in quadruple precision, \f$\|G\|_F^2\f$ or \f$\|\mathop{\mathrm{off}}(G)\|_F^2\f$ of a square extended precision real matrix \f$G\f$ of order \f$N\f$.
!!
!!@param O [IN]; \f$O\in\{\mathrm{'A'},\mathrm{'a'},\mathrm{'O'},\mathrm{'o'}\}\f$, where \f$\mathrm{'a'}\f$ and \f$\mathrm{'o'}\f$ select either the whole input matrix or its off-diagonal part, respectively, while the upper-case arguments request OpenMP computation.
!!@param N [IN]; \f$N\f$ is the matrix order.
!!@param G [IN]; \f$G\f$ is a square extended precision real matrix of order \f$N\f$.
!!@param LDG [IN]; the leading dimension of \f$G\f$.
!!@param S [OUT]; the computed square of the Frobenius norm.
!!@param INFO [OUT]; zero on success or \f$-i\f$ if the \f$i\f$th argument had an illegal value.
SUBROUTINE QXOFFSQ(O, N, G, LDG, S, INFO)
#ifdef USE_IEEE_INTRINSIC
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDG
  REAL(KIND=10), INTENT(IN) :: G(LDG,N)
  REAL(KIND=REAL128), INTENT(OUT) :: S
  INTEGER, INTENT(OUT) :: INFO
  REAL(KIND=REAL128) :: Y
  INTEGER :: I, J
  S = 0.0_REAL128
#ifdef USE_IEEE_INTRINSIC
#include "goffsqi.F90"
#else
#include "goffsq.F90"
#endif
END SUBROUTINE QXOFFSQ
