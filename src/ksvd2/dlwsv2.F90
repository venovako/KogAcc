!>@brief \b DLWSV2 computes the SVD of an upper triangular double precision 2x2 matrix G as G = U S V^T by calling a modernized LAPACK's DLASV2 routine.
!!
!!@param G [IN]; G is an upper triangular 2x2 double precision matrix.
!!@param U [OUT]; U is an orthogonal double precision matrix of order two.
!!@param V [OUT]; V is an orthogonal double precision matrix of order two.
!!@param S [OUT]; S is a double precision array with two elements, s_{11} and s_{22}.
!!@param INFO [OUT]; On success, 0; else, if G is not upper triangular, -HUGE(INFO)-1 (and U=V=I, s_{11}=s_{22}=0).
SUBROUTINE DLWSV2(G, U, V, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
#ifndef __GFORTRAN__
  INTERFACE
     SUBROUTINE DLMSV2(F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       REAL(KIND=REAL64), INTENT(IN) :: F, G, H
       REAL(KIND=REAL64), INTENT(OUT) :: SSMIN, SSMAX, SNR, CSR, SNL, CSL
     END SUBROUTINE DLMSV2
  END INTERFACE
#endif
  INTEGER, PARAMETER :: K = REAL64, IERR = -HUGE(0)-1
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K

  REAL(KIND=K), INTENT(IN) :: G(2,2)
  REAL(KIND=K), INTENT(OUT) :: U(2,2), V(2,2), S(2)
  INTEGER, INTENT(OUT) :: INFO
#ifdef __GFORTRAN__
  EXTERNAL :: DLMSV2
#endif
  U(1,1) = ONE
  U(2,1) = ZERO
  U(1,2) = ZERO
  U(2,2) = ONE
  V(1,1) = ONE
  V(2,1) = ZERO
  V(1,2) = ZERO
  V(2,2) = ONE
  S(1) = ZERO
  S(2) = ZERO

  IF (G(2,1) .EQ. ZERO) THEN
     CALL DLMSV2(G(1,1), G(1,2), G(2,2), S(2), S(1), V(2,1), V(1,1), U(2,1), U(1,1))
     U(1,2) = -U(2,1)
     U(2,2) =  U(1,1)
     V(1,2) = -V(2,1)
     V(2,2) =  V(1,1)
     INFO = 0
  ELSE ! G is not upper triangular
     INFO = IERR
  END IF
END SUBROUTINE DLWSV2
