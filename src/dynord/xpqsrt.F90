!>@brief \b XPQSRT sorts the arrays AW,AP,AQ by the merge sort algorithm, using the workspace arrays BW,BP,BQ of the same length N, according to the ordering defined by the WPQCMP subroutine, and returns a non-negative value in INFO if successful, or a negative value on failure.
SUBROUTINE XPQSRT(WPQCMP, N, AW, AP, AQ, BW, BP, BQ, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE

  ABSTRACT INTERFACE
     PURE SUBROUTINE PQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
       REAL(KIND=c_long_double), INTENT(IN) :: XW, YW
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE PQCMP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XPQMRG(WPQCMP, M, N, AW, AP, AQ, BW, BP, BQ, XW, XP, XQ, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       ABSTRACT INTERFACE
          PURE SUBROUTINE PQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
            REAL(KIND=c_long_double), INTENT(IN) :: XW, YW
            INTEGER, INTENT(OUT) :: INFO
          END SUBROUTINE PQCMP
       END INTERFACE
       INTEGER, INTENT(IN) :: M, N, AP(M), AQ(M), BP(N), BQ(N)
       REAL(KIND=c_long_double), INTENT(IN) :: AW(M), BW(N)
       REAL(KIND=c_long_double), INTENT(OUT) :: XW(M+N)
       INTEGER, INTENT(OUT) :: XP(M+N), XQ(M+N), INFO
       PROCEDURE(PQCMP) :: WPQCMP
     END SUBROUTINE XPQMRG
  END INTERFACE

  INTEGER, INTENT(IN) :: N
  REAL(KIND=c_long_double), INTENT(INOUT) :: AW(N)
  INTEGER, INTENT(INOUT) :: AP(N), AQ(N), INFO
  REAL(KIND=c_long_double), INTENT(OUT) :: BW(N)
  INTEGER, INTENT(OUT) :: BP(N), BQ(N)
  PROCEDURE(PQCMP) :: WPQCMP
  INTEGER :: I, J, K, L, M, X, Y
  LOGICAL :: FLIP
#define WPQMRG XPQMRG
#include "gpqsrt.F90"
END SUBROUTINE XPQSRT
