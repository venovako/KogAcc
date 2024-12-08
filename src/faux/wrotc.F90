!>@brief \b WROTC postmultiplies the columns (p,q) of G by W.
SUBROUTINE WROTC(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  IMPLICIT NONE
#define CR_HYPOT HYPOT
  INTERFACE
     FUNCTION WMUL(A, B)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B
       COMPLEX(KIND=c_long_double) :: WMUL
     END FUNCTION WMUL
  END INTERFACE
  INTERFACE
     FUNCTION WFMA(A, B, C)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       COMPLEX(KIND=c_long_double), INTENT(IN) :: A, B, C
       COMPLEX(KIND=c_long_double) :: WFMA
     END FUNCTION WFMA
  END INTERFACE
  INTEGER, PARAMETER :: K = c_long_double
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
#define VL 1
  COMPLEX(KIND=K) :: X(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: X
  COMPLEX(KIND=K) :: Y(VL)
  !DIR$ ATTRIBUTES ALIGN: 64:: Y
  INTEGER :: I, J
#define MUL WMUL
#define FMA WFMA
#include "hrotc.F90"
END SUBROUTINE WROTC
