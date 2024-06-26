!>@brief \b YKSVD0 computes the SVD of G as U S V^H, with S returned in SV and U and V optionally accumulated on either identity for the SVD, or on preset input matrices.
SUBROUTINE YKSVD0(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, O, R, INFO)
#ifdef NDEBUG
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL128
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL128, OUTPUT_UNIT
#endif
  !$ USE OMP_LIB
  IMPLICIT NONE

#define CR_HYPOT HYPOT
  INTERFACE
     SUBROUTINE YLANGO(N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDG
       COMPLEX(KIND=REAL128), INTENT(IN) :: G(N,LDG)
       REAL(KIND=REAL128), INTENT(OUT) :: S
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE YLANGO
  END INTERFACE
  INTERFACE
     SUBROUTINE YSCALG(M, N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, S
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE YSCALG
  END INTERFACE
  INTERFACE
     SUBROUTINE YMK3PQ(K, N, G, LDG, W, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: K, N, LDG
       COMPLEX(KIND=REAL128), INTENT(IN) :: G(LDG,N)
       REAL(KIND=REAL128), INTENT(OUT) :: W(N*N)
       INTEGER, INTENT(OUT) :: O(2*N*(N-1))
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE YMK3PQ
  END INTERFACE
  INTERFACE
     SUBROUTINE YKSVD2(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       COMPLEX(KIND=REAL128), INTENT(IN) :: G(2,2)
       COMPLEX(KIND=REAL128), INTENT(OUT) :: U(2,2), V(2,2)
       REAL(KIND=REAL128), INTENT(OUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE YKSVD2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE YCVGPP(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       COMPLEX(KIND=REAL128), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
       REAL(KIND=REAL128), INTENT(INOUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE YCVGPP
  END INTERFACE
  INTERFACE
     SUBROUTINE YROTC(M, N, G, LDG, P, Q, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, P, Q
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL128), INTENT(IN) :: W(2,2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE YROTC
  END INTERFACE
  INTERFACE
     SUBROUTINE YROTR(M, N, G, LDG, P, Q, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, P, Q
       COMPLEX(KIND=REAL128), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL128), INTENT(IN) :: W(2,2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE YROTR
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N, S, T, P, O(*)
       INTEGER, INTENT(OUT) :: R(2,P), INFO
     END SUBROUTINE JSTEP
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL128, USID = 8, UACC = 16, VSID = 32, VACC = 64
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  COMPLEX(KIND=K), PARAMETER :: CZERO = (ZERO,ZERO), CONE = (ONE,ZERO)
  INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
  REAL(KIND=REAL128), INTENT(OUT) :: SV(N)
  REAL(KIND=K), INTENT(INOUT) :: W(MAX(N,5)*N)
  INTEGER, INTENT(INOUT) :: O(2*N*(N-1)), INFO
  INTEGER, INTENT(OUT) :: R(2,*)

  COMPLEX(KIND=K) :: G2(2,2), U2(2,2), V2(2,2)
  REAL(KIND=K) :: GN, UN, VN
  INTEGER(KIND=INT64) :: TT, TM, SM
  INTEGER :: MRQSTP, I, J, L, M, M_2, NP, NS, P, Q, T, JS, GS, US, VS, WV, WS, STP, XSG, XSU, XSV, ES(3)
  LOGICAL :: LOMP, LUSID, LUACC, LVSID, LVACC

#define LANGO YLANGO
#define SCALG YSCALG
#define MK3PQ YMK3PQ
#define KSVD2 YKSVD2
#define CVGPP YCVGPP
#define ROTC YROTC
#define ROTR YROTR
#undef ANIMATE
#include "hksvd0.F90"
#ifndef NDEBUG
9 FORMAT(A,ES45.36E4)
#endif
END SUBROUTINE YKSVD0
