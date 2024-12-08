!>@brief \b XMK3PQ builds at most K pivot index pairs for the next transformation of G.
SUBROUTINE XMK3PQ(K, N, G, LDG, W, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE

#define CR_HYPOT HYPOT
  INTERFACE
     SUBROUTINE XABSG(M, N, G, LDG, W, LDW, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, LDW
       REAL(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(OUT) :: W(LDW,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE XABSG
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XMKWPQ(N, G, LDG, W, O, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDG
       REAL(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
       REAL(KIND=c_long_double), INTENT(INOUT) :: W(N,N)
       INTEGER, INTENT(OUT) :: O(N*(N-1)), INFO
     END SUBROUTINE XMKWPQ
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE XPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
       REAL(KIND=c_long_double), INTENT(IN) :: XW, YW
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XPQCMP
  END INTERFACE
  INTERFACE
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
       INTEGER, INTENT(IN) :: N
       REAL(KIND=c_long_double), INTENT(INOUT) :: AW(N)
       INTEGER, INTENT(INOUT) :: AP(N), AQ(N), INFO
       REAL(KIND=c_long_double), INTENT(OUT) :: BW(N)
       INTEGER, INTENT(OUT) :: BP(N), BQ(N)
       PROCEDURE(PQCMP) :: WPQCMP
     END SUBROUTINE XPQSRT
  END INTERFACE

  REAL(KIND=c_long_double), PARAMETER :: ZERO = 0.0_c_long_double, MONE = -1.0_c_long_double
  INTEGER, INTENT(IN) :: K, N, LDG
  REAL(KIND=c_long_double), INTENT(IN) :: G(LDG,N)
  REAL(KIND=c_long_double), INTENT(OUT) :: W(N*N)
  INTEGER, INTENT(OUT) :: O(2*N*(N-1))
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J, L, M, M_2
#define ABSG XABSG
#define MKWPQ XMKWPQ
#define PQCMP XPQCMP
#define PQSRT XPQSRT
#include "gmk3pq.F90"
END SUBROUTINE XMK3PQ
