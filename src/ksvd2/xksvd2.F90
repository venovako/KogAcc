!>@brief \b XKSVD2 computes the SVD of an extended precision 2x2 matrix G as G = U S V^T, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 extended precision matrix with finite elements.
!!@param U [OUT]; U is an orthogonal extended precision matrix of order two.
!!@param V [OUT]; V is an orthogonal extended precision matrix of order two.
!!@param S [OUT]; S' is a extended precision array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [INOUT]; do not set to anything but zero on input unless the effects are understood; on output, the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite element, then s=-HUGE(s)-1.
SUBROUTINE XKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  IMPLICIT NONE

  INTEGER, PARAMETER :: K = 10
  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(INOUT) :: INFO(3)
  INTEGER(c_int), EXTERNAL :: PVN_XLJSV2

  INTEGER(c_int) :: ES(3), KND
#define LJSV2 PVN_XLJSV2
#include "gksvd2.F90"
END SUBROUTINE XKSVD2
