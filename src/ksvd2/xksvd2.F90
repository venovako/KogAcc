!>@brief \b XKSVD2 computes the SVD of an extended precision 2x2 matrix G as G = U S V^T, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 extended precision matrix with finite elements.
!!@param U [OUT]; U is an orthogonal extended precision matrix of order two.
!!@param V [OUT]; V is an orthogonal extended precision matrix of order two.
!!@param S [OUT]; S' is a extended precision array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [INOUT]; do not set to anything but zero on input unless the effects are understood; on output, the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite element, then s=-HUGE(s).
SUBROUTINE XKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  IMPLICIT NONE

  INTEGER, PARAMETER :: K = 10
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K
  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER(c_int), EXTERNAL :: PVN_XLJSV2

  INTEGER(c_int) :: ES(3), KND

  ES(1) = INT(INFO, c_int)
  KND = PVN_XLJSV2(G(1,1), G(2,1), G(1,2), G(2,2), &
       U(1,1), U(2,1), U(1,2), U(2,2), V(1,1), V(2,1), V(1,2), V(2,2), S(1), S(2), ES)
  IF (KND .LT. 0_c_int) THEN
     INFO = -HUGE(INFO)
  ELSE ! all OK
     INFO = MAX(0, EXPONENT(TINY(ZERO)) - INT(ES(2) - ES(1)))
     INFO = MIN(INFO, EXPONENT(HUGE(ZERO)) - INT(ES(3) - ES(1)))
     S(1) = SCALE(S(1), INT(ES(2) - ES(1)) + INFO)
     S(2) = SCALE(S(2), INT(ES(3) - ES(1)) + INFO)
  END IF
END SUBROUTINE XKSVD2
