!>@brief \b SBROTR premultiplies the block-rows (P,Q) of G by R.
SUBROUTINE SBROTR(M, B, G, LDG, P, Q, R, LDB, W, LDW, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
#ifdef LAPACK
  INTERFACE
     PURE SUBROUTINE SGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: TRANSA, TRANSB
       INTEGER, INTENT(IN) :: M, N, K, LDA, LDB, LDC
       REAL(KIND=REAL32), INTENT(IN) :: A(LDA,K), B(LDB,N), ALPHA, BETA
       REAL(KIND=REAL32), INTENT(INOUT) :: C(LDC,N)
     END SUBROUTINE SGEMM
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SLACPY(UPLO, M, N, A, LDA, B, LDB)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: UPLO
       INTEGER, INTENT(IN) :: M, N, LDA, LDB
       REAL(KIND=REAL32), INTENT(IN) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(OUT) :: B(LDB,N)
     END SUBROUTINE SLACPY
  END INTERFACE
#else
  INTERFACE
     PURE SUBROUTINE SMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: TRANSA, TRANSB
       INTEGER, INTENT(IN) :: M, N, K, LDA, LDB, LDC
       REAL(KIND=REAL32), INTENT(IN) :: A(LDA,K), B(LDB,N), ALPHA, BETA
       REAL(KIND=REAL32), INTENT(INOUT) :: C(LDC,N)
     END SUBROUTINE SMM
  END INTERFACE
  INTERFACE
     PURE SUBROUTINE SCP(UPLO, M, N, A, LDA, B, LDB)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       CHARACTER, INTENT(IN) :: UPLO
       INTEGER, INTENT(IN) :: M, N, LDA, LDB
       REAL(KIND=REAL32), INTENT(IN) :: A(LDA,N)
       REAL(KIND=REAL32), INTENT(OUT) :: B(LDB,N)
     END SUBROUTINE SCP
  END INTERFACE
#endif
  INTEGER, PARAMETER :: K = REAL32
  REAL(KIND=K), PARAMETER :: ZERO = 0.0_K, ONE = 1.0_K
  INTEGER, INTENT(IN) :: M, B, LDG, P, Q, LDB, LDW
  REAL(KIND=K), INTENT(INOUT) :: G(LDG,M)
  REAL(KIND=K), INTENT(IN) :: R(LDB,2*B)
  REAL(KIND=K), INTENT(OUT) :: W(LDW,B)
  INTEGER, INTENT(INOUT) :: INFO
  INTEGER :: I, J, GI, GJ, RI, RJ
#ifdef LAPACK
#define GEMM SGEMM
#define LACPY SLACPY
#else
#define GEMM SMM
#define LACPY SCP
#endif
#include "gbrotr.F90"
END SUBROUTINE SBROTR
