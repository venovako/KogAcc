!>@brief \b DKSVD2 computes the SVD of a double precision 2x2 matrix G as G = U S V^T, assuming rounding to nearest.
!!
!!@param G [IN]; G is a general 2x2 double precision matrix with finite elements.
!!@param U [OUT]; U is an orthogonal double precision matrix of order two.
!!@param V [OUT]; V is an orthogonal double precision matrix of order two.
!!@param S [OUT]; S' is a double precision array with two elements, s_{11}' and s_{22}', both non-negative and finite.
!!@param INFO [INOUT]; do not set to anything but zero on input unless the effects are understood; on output, the scaling parameter s such that 2^{-s} S' = S.
!!If G has a non-finite element, then s=-HUGE(s).
SUBROUTINE DKSVD2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_int
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  INTEGER, PARAMETER :: K = REAL64
  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER(c_int), EXTERNAL :: PVN_DLJSV2

  INTEGER(c_int) :: ES, KND

  ES = INT(INFO, c_int)
  KND = PVN_DLJSV2(G(1,1), G(2,1), G(1,2), G(2,2), &
       U(1,1), U(2,1), U(1,2), U(2,2), V(1,1), V(2,1), V(1,2), V(2,2), S(1), S(2), ES)
  IF (KND .LT. 0) THEN
     INFO = -HUGE(INFO)
  ELSE ! all OK
     INFO = INT(ES)
  END IF
END SUBROUTINE DKSVD2
