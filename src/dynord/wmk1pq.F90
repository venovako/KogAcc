!>@brief \b WMK1PQ builds at most one \f$(p,q)\f$ pivot index pair for the next transformation of \f$G\f$.
SUBROUTINE WMK1PQ(K, N, G, LDG, W, O, INFO)
  IMPLICIT NONE
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  INTEGER, INTENT(IN) :: K, N, LDG
  COMPLEX(KIND=10), INTENT(IN) :: G(LDG,N)
  REAL(KIND=10), INTENT(OUT) :: W(N*N)
  INTEGER, INTENT(OUT) :: O(2*N*(N-1)), INFO
  INTEGER :: M, M_2
  EXTERNAL :: WABSG, XMKWPQ, XPQSRT
#define ABSG WABSG
#define MKWPQ XMKWPQ
#define PQSRT XPQSRT
#include "gmk1pq.F90"
END SUBROUTINE WMK1PQ
