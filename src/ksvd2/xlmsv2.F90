!> \brief \b XLMSV2 computes the singular value decomposition of a 2-by-2 triangular matrix.
!
!  =========== DOCUMENTATION ===========
!
! Online html documentation available at
!            http://www.netlib.org/lapack/explore-html/
!
! MODIFIED BY VENOVAKO
!
!  Definition:
!  ===========
!
!       SUBROUTINE XLMSV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL )
!
!       .. Scalar Arguments ..
!       REAL(KIND=10)      CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN
!       ..
!
!
!> \par Purpose:
!  =============
!>
!> \verbatim
!>
!> XLMSV2 computes the singular value decomposition of a 2-by-2
!> triangular matrix
!>    [  F   G  ]
!>    [  0   H  ].
!> On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
!> smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
!> right singular vectors for abs(SSMAX), giving the decomposition
!>
!>    [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
!>    [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
!> \endverbatim
!
!  Arguments:
!  ==========
!
!> \param[in] F
!> \verbatim
!>          F is REAL
!>          The (1,1) element of the 2-by-2 matrix.
!> \endverbatim
!>
!> \param[in] G
!> \verbatim
!>          G is REAL
!>          The (1,2) element of the 2-by-2 matrix.
!> \endverbatim
!>
!> \param[in] H
!> \verbatim
!>          H is REAL
!>          The (2,2) element of the 2-by-2 matrix.
!> \endverbatim
!>
!> \param[out] SSMIN
!> \verbatim
!>          SSMIN is REAL
!>          abs(SSMIN) is the smaller singular value.
!> \endverbatim
!>
!> \param[out] SSMAX
!> \verbatim
!>          SSMAX is REAL
!>          abs(SSMAX) is the larger singular value.
!> \endverbatim
!>
!> \param[out] SNL
!> \verbatim
!>          SNL is REAL
!> \endverbatim
!>
!> \param[out] CSL
!> \verbatim
!>          CSL is REAL
!>          The vector (CSL, SNL) is a unit left singular vector for the
!>          singular value abs(SSMAX).
!> \endverbatim
!>
!> \param[out] SNR
!> \verbatim
!>          SNR is REAL
!> \endverbatim
!>
!> \param[out] CSR
!> \verbatim
!>          CSR is REAL
!>          The vector (CSR, SNR) is a unit right singular vector for the
!>          singular value abs(SSMAX).
!> \endverbatim
!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee
!> \author Univ. of California Berkeley
!> \author Univ. of Colorado Denver
!> \author NAG Ltd.
!
!> \ingroup lasv2
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>  Any input parameter may be aliased with any output parameter.
!>
!>  Barring over/underflow and assuming a guard digit in subtraction, all
!>  output quantities are correct to within a few units in the last
!>  place (ulps).
!>
!>  In IEEE arithmetic, the code works correctly if one matrix element is
!>  infinite.
!>
!>  Overflow will not occur unless the largest singular value itself
!>  overflows or is within a few ulps of overflow.
!>
!>  Underflow is harmless if underflow is gradual. Otherwise, results
!>  may correspond to a matrix modified by perturbations of size near
!>  the underflow threshold.
!> \endverbatim
!>
!  =====================================================================
PURE SUBROUTINE XLMSV2(F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL)
#ifdef USE_IEEE_INTRINSIC
#if ((USE_IEEE_INTRINSIC & 48) == 0)
#undef USE_IEEE_INTRINSIC
#elif ((USE_IEEE_INTRINSIC & 48) == 16)
  USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_FMA
#endif
#endif
  IMPLICIT NONE
#ifdef USE_IEEE_INTRINSIC
#if ((USE_IEEE_INTRINSIC & 48) == 32)
  INTERFACE
     PURE FUNCTION IEEE_FMA(X, Y, Z) BIND(C,NAME='fmal')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       REAL(KIND=c_long_double), INTENT(IN), VALUE :: X, Y, Z
       REAL(KIND=c_long_double) :: IEEE_FMA
     END FUNCTION IEEE_FMA
  END INTERFACE
#endif
#endif
  INTEGER, PARAMETER :: K = 10
!
!  -- LAPACK auxiliary routine --
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!
!     .. Scalar Arguments ..
  REAL(KIND=K), INTENT(IN) :: F, G, H
  REAL(KIND=K), INTENT(OUT) :: CSL, CSR, SNL, SNR, SSMAX, SSMIN
!     ..
!
! =====================================================================
!
!     .. Parameters ..
  REAL(KIND=K), PARAMETER :: ZERO = 0.0E0_K
  REAL(KIND=K), PARAMETER :: HALF = 0.5E0_K
  REAL(KIND=K), PARAMETER :: ONE = 1.0E0_K
  REAL(KIND=K), PARAMETER :: TWO = 2.0E0_K
  REAL(KIND=K), PARAMETER :: FOUR = 4.0E0_K
!     ..
!     .. Local Scalars ..
  LOGICAL :: GASMAL, SWAP
  INTEGER :: PMAX
  REAL(KIND=K) :: A, CLT, CRT, D, FA, FT, GA, GT, HA, HT, L, M, MM, R, S, SLT, SRT, T, TEMP, TSIGN, TT
!     ..
!     .. Executable Statements ..
!
  FT = F
  FA = ABS(FT)
  HT = H
  HA = ABS(H)
!
!     PMAX points to the maximum absolute element of matrix
!       PMAX = 1 if F largest in absolute values
!       PMAX = 2 if G largest in absolute values
!       PMAX = 3 if H largest in absolute values
!
  PMAX = 1
  SWAP = (HA .GT. FA)
  IF (SWAP) THEN
     PMAX = 3
     TEMP = FT
     FT = HT
     HT = TEMP
     TEMP = FA
     FA = HA
     HA = TEMP
!
!        Now FA .ge. HA
!
  END IF
  GT = G
  GA = ABS(GT)
  IF (GA .EQ. ZERO) THEN
!
!        Diagonal matrix
!
     SSMIN = HA
     SSMAX = FA
     CLT = ONE
     CRT = ONE
     SLT = ZERO
     SRT = ZERO
  ELSE
     GASMAL = .TRUE.
     IF (GA .GT. FA) THEN
        PMAX = 2
        IF ((FA / GA) .LT. (EPSILON(ONE) * HALF)) THEN
!
!              Case of very large GA
!
           GASMAL = .FALSE.
           SSMAX = GA
           IF (HA .GT. ONE) THEN
              SSMIN = FA / (GA / HA)
           ELSE
              SSMIN = (FA / GA) * HA
           END IF
           CLT = ONE
           SLT = HT / GT
           SRT = ONE
           CRT = FT / GT
        END IF
     END IF
     IF (GASMAL) THEN
!
!           Normal case
!
        D = FA - HA
        IF (D .EQ. FA) THEN
!
!              Copes with infinite F or H
!
           L = ONE
        ELSE
           L = D / FA
        END IF
!
!           Note that 0 .le. L .le. 1
!
        M = GT / FT
!
!           Note that abs(M) .le. 1/macheps
!
        T = TWO - L
!
!           Note that T .ge. 1
!
        MM = M * M
        TT = T * T
        S = SQRT(TT + MM)
!
!           Note that 1 .le. S .le. 1 + 1/macheps
!
        IF (L .EQ. ZERO) THEN
           R = ABS(M)
        ELSE
#ifdef USE_IEEE_INTRINSIC
           R = SQRT(IEEE_FMA(L, L, MM))
#else
           R = SQRT(L * L + MM)
#endif
        END IF
!
!           Note that 0 .le. R .le. 1 + 1/macheps
!
        A = HALF * (S + R)
!
!           Note that 1 .le. A .le. 1 + abs(M)
!
        SSMIN = HA / A
        SSMAX = FA * A
        IF (MM .EQ. ZERO) THEN
!
!              Note that M is very tiny
!
           IF (L .EQ. ZERO) THEN
              T = SIGN(TWO, FT) * SIGN(ONE, GT)
           ELSE
              T = GT / SIGN(D, FT) + M / T
           END IF
        ELSE
           T = (M / (S + T) + M / (R + L)) * (ONE + A)
        END IF
#ifdef USE_IEEE_INTRINSIC
        L = SQRT(IEEE_FMA(T, T, FOUR))
#else
        L = SQRT(T * T + FOUR)
#endif
        CRT = TWO / L
        SRT = T / L
#ifdef USE_IEEE_INTRINSIC
        CLT = IEEE_FMA(SRT, M, CRT) / A
#else
        CLT = (SRT * M + CRT) / A
#endif
        SLT = (HT / FT) * SRT / A
     END IF
  END IF
  IF (SWAP) THEN
     CSL = SRT
     SNL = CRT
     CSR = SLT
     SNR = CLT
  ELSE
     CSL = CLT
     SNL = SLT
     CSR = CRT
     SNR = SRT
  END IF
!
!     Correct signs of SSMAX and SSMIN
!
  IF (PMAX .EQ. 1) TSIGN = SIGN(ONE, CSR) * SIGN(ONE, CSL) * SIGN(ONE, F)
  IF (PMAX .EQ. 2) TSIGN = SIGN(ONE, SNR) * SIGN(ONE, CSL) * SIGN(ONE, G)
  IF (PMAX .EQ. 3) TSIGN = SIGN(ONE, SNR) * SIGN(ONE, SNL) * SIGN(ONE, H)
  SSMAX = SIGN(SSMAX, TSIGN)
  SSMIN = SIGN(SSMIN, TSIGN * SIGN(ONE, F) * SIGN(ONE, H))
!
!     End of XLMSV2
!
END SUBROUTINE XLMSV2
