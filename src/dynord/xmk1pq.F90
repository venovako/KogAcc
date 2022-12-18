!>@brief \b XMK1PQ builds at most one \f$(p,q)\f$ pivot index pair for the next transformation of \f$G\f$.
SUBROUTINE XMK1PQ(K, N, G, LDG, W, O, INFO)
  IMPLICIT NONE
  REAL(KIND=10), PARAMETER :: ZERO = 0.0_10
  INTEGER, INTENT(IN) :: K, N, LDG
  REAL(KIND=10), INTENT(IN) :: G(LDG,N)
  REAL(KIND=10), INTENT(OUT) :: W(N*N)
  INTEGER, INTENT(OUT) :: O(2*N*(N-1)), INFO
  INTEGER :: M, M_2
  EXTERNAL :: XABSG, XMKWPQ, XPQSRT
#define ABSG XABSG
#define MKWPQ XMKWPQ
#define PQSRT XPQSRT
#include "gmk1pq.F90"
END SUBROUTINE XMK1PQ
