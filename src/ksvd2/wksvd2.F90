!>@brief \b WKSVD2 computes the SVD of an extended precision complex 2x2 matrix G as G = U S V^H, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 extended precision complex matrix with all components of its elements finite.
!!@param U [OUT]; U is a unitary extended precision complex matrix of order two.
!!@param V [OUT]; V is a unitary extended precision complex matrix of order two.
!!@param S [OUT]; S' is a extended precision real array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [INOUT]; do not set to anything but zero on input unless the effects are understood; on output, the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite component, then s=-HUGE(s)-1.
SUBROUTINE WKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, PARAMETER :: K = c_long_double
  COMPLEX(KIND=K), INTENT(IN) :: G(2,2)
  COMPLEX(KIND=K), INTENT(OUT) :: U(2,2), V(2,2)
  REAL(KIND=K), INTENT(OUT) :: S(2)
  INTEGER, INTENT(INOUT) :: INFO(3)
  INTEGER(c_int), EXTERNAL :: PVN_WLJSV2

  REAL(KIND=K) :: G11R, G11I, G21R, G21I, G12R, G12I, G22R, G22I
  REAL(KIND=K) :: U11R, U11I, U21R, U21I, U12R, U12I, U22R, U22I
  REAL(KIND=K) :: V11R, V11I, V21R, V21I, V12R, V12I, V22R, V22I
  INTEGER(c_int) :: ES(3), KND
#define LJSV2 PVN_WLJSV2
#include "hksvd2.F90"
END SUBROUTINE WKSVD2
