PURE FUNCTION ZMUL(A, B)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE PVN_ZMUL(CR, CI, AR, AI, BR, BI)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_double
       IMPLICIT NONE
       REAL(KIND=c_double), INTENT(OUT) :: CR, CI
       REAL(KIND=c_double), INTENT(IN) :: AR, AI, BR, BI
     END SUBROUTINE PVN_ZMUL
  END INTERFACE
  INTEGER, PARAMETER :: K = REAL64
  COMPLEX(KIND=K), INTENT(IN) :: A, B
  COMPLEX(KIND=K) :: ZMUL
  REAL(KIND=K) :: AR, AI, BR, BI, CR, CI
#define KMUL ZMUL
#define PMUL PVN_ZMUL
#include "hmul.F90"
END FUNCTION ZMUL
