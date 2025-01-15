  PURE FUNCTION XENCAP(H, P, Q)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
#ifdef HAVE_UNSIGNED
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: UINT8
#else
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT8
#endif
    IMPLICIT NONE
    REAL(KIND=c_long_double), INTENT(IN) :: H
    INTEGER, INTENT(IN) :: P, Q
    REAL(KIND=c_long_double) :: XENCAP, W
#ifdef HAVE_UNSIGNED
    UNSIGNED(KIND=UINT8) :: B(16)
#else
    INTEGER(KIND=INT8) :: B(16)
#endif
    EQUIVALENCE(W, B)
    W = H
#ifdef HAVE_UNSIGNED
    B(1) = UINT((Q - 1), UINT8)
    B(2) = UINT((P - 1), UINT8)
#else
    B(1) = INT((Q - 1), INT8)
    B(2) = INT((P - 1), INT8)
#endif
    XENCAP = W
  END FUNCTION XENCAP
