!>@brief \b ZKSVD0 computes the SVD of G as U S V^H, with S returned in SV and U and V optionally accumulated on either identity for the SVD, or on preset input matrices.
SUBROUTINE ZKSVD0(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, O, R, INFO)
#ifdef ANIMATE
  USE, INTRINSIC :: ISO_C_BINDING
#endif
#ifdef PRINT0UT
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64, PRINT0UT
#else
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL64
#endif
  !$ USE OMP_LIB
  IMPLICIT NONE
#ifdef CR_MATH
  INTERFACE
     PURE FUNCTION CR_HYPOT(X, Y) BIND(C,NAME='cr_hypot')
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       REAL(KIND=c_double), INTENT(IN), VALUE :: X, Y
       REAL(KIND=c_double) :: CR_HYPOT
     END FUNCTION CR_HYPOT
  END INTERFACE
#else
#define CR_HYPOT HYPOT
#endif
  INTERFACE
     SUBROUTINE ZLANGO(N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, LDG
       COMPLEX(KIND=REAL64), INTENT(IN) :: G(N,LDG)
       REAL(KIND=REAL64), INTENT(OUT) :: S
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZLANGO
  END INTERFACE
  INTERFACE
     SUBROUTINE ZSCALG(M, N, G, LDG, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, S
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZSCALG
  END INTERFACE
  INTERFACE
     SUBROUTINE ZKSVD2(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       COMPLEX(KIND=REAL64), INTENT(IN) :: G(2,2)
       COMPLEX(KIND=REAL64), INTENT(OUT) :: U(2,2), V(2,2)
       REAL(KIND=REAL64), INTENT(OUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE ZKSVD2
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZCVGPP(G, U, V, S, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       COMPLEX(KIND=REAL64), INTENT(IN) :: G(2,2), U(2,2), V(2,2)
       REAL(KIND=REAL64), INTENT(INOUT) :: S(2)
       INTEGER, INTENT(INOUT) :: INFO(3)
     END SUBROUTINE ZCVGPP
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZROTC(M, N, G, LDG, P, Q, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, P, Q
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL64), INTENT(IN) :: W(2,2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZROTC
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE ZROTR(M, N, G, LDG, P, Q, W, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG, P, Q
       COMPLEX(KIND=REAL64), INTENT(INOUT) :: G(LDG,N)
       COMPLEX(KIND=REAL64), INTENT(IN) :: W(2,2)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE ZROTR
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE JSTEP(J, N, S, T, P, O, R, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N, S, T, P, O(*)
       INTEGER, INTENT(OUT) :: R(2,P), INFO
     END SUBROUTINE JSTEP
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL64, USID = 64, UACC = 128, VSID = 256, VACC = 512
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  COMPLEX(KIND=K), PARAMETER :: CZERO = (ZERO,ZERO), CONE = (ONE,ZERO)
  INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV, O(*)
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
#ifdef ANIMATE
  REAL(KIND=K), INTENT(INOUT), TARGET :: SV(N)
#else
  REAL(KIND=K), INTENT(OUT) :: SV(N)
#endif
  REAL(KIND=K), INTENT(OUT) :: W(*)
  INTEGER, INTENT(OUT) :: R(2,*)
  INTEGER, INTENT(INOUT) :: INFO

  COMPLEX(KIND=K) :: G2(2,2), U2(2,2), V2(2,2)
  REAL(KIND=K) :: GN, UN, VN
  INTEGER(KIND=INT64) :: TM, TT
  INTEGER :: MRQSTP, I, J, L, M, M_2, NP, NS, P, Q, T, JS, GS, US, VS, WV, WS, STP, ES(3)
  LOGICAL :: LOMP, LACC, LUSID, LUACC, LVSID, LVACC
#ifdef ANIMATE
  TYPE(c_ptr) :: CTX
  TYPE(c_ptr), POINTER :: CP
  INTEGER(KIND=c_size_t) :: LDF
  INTEGER(KIND=c_int), EXTERNAL :: PVN_CVIS_FRAME
#endif

#define LANGO ZLANGO
#define SCALG ZSCALG
#define KSVD2 ZKSVD2
#define CVGPP ZCVGPP
#define ROTC ZROTC
#define ROTR ZROTR
#include "hksvd0.F90"
#ifdef PRINT0UT
9 FORMAT(A,ES25.17E3)
#endif
END SUBROUTINE ZKSVD0
