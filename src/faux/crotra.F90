!>@brief \b CROTRA premultiplies the rows (p,q) of G by W using an imperfect emulation of an accurate a*b+c*d operation.
PURE SUBROUTINE CROTRA(M, N, G, LDG, P, Q, W, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
  IMPLICIT NONE
  INTEGER, PARAMETER :: K = REAL32
  INTEGER, INTENT(IN) :: M, N, LDG, P, Q
  COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
  COMPLEX(KIND=K), INTENT(IN) :: W(2,2)
  INTEGER, INTENT(INOUT) :: INFO
  IF (MOD(N, 8) .EQ. 0) THEN
     CALL CROTRA8(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(N, 4) .EQ. 0) THEN
     CALL CROTRA4(M, N, G, LDG, P, Q, W, INFO)
  ELSE IF (MOD(N, 2) .EQ. 0) THEN
     CALL CROTRA2(M, N, G, LDG, P, Q, W, INFO)
  ELSE ! error
     INFO = -2
  END IF
CONTAINS
  PURE SUBROUTINE CROTRA2(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32, L = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
    COMPLEX(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 2
#undef HL
#define HL 1
#include "hrotra.F90"
  END SUBROUTINE CROTRA2
  PURE SUBROUTINE CROTRA4(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32, L = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
    COMPLEX(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 4
#undef HL
#define HL 2
#include "hrotra.F90"
  END SUBROUTINE CROTRA4
  PURE SUBROUTINE CROTRA8(M, N, G, LDG, P, Q, W, INFO)
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64
    IMPLICIT NONE
    INTEGER, PARAMETER :: K = REAL32, L = REAL64
    INTEGER, INTENT(IN) :: M, N, LDG, P, Q
    COMPLEX(KIND=K), INTENT(INOUT) :: G(LDG,N)
    COMPLEX(KIND=K), INTENT(IN) :: W(2,2)
    INTEGER, INTENT(INOUT) :: INFO
#undef VL
#define VL 8
#undef HL
#define HL 4
#include "hrotra.F90"
  END SUBROUTINE CROTRA8
END SUBROUTINE CROTRA
