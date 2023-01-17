!>@brief \b XPQMRG merges the arrays AW,AP,AQ (of length M) and BW,BP,BQ (of length N) into the arrays XW,XP,XQ (of length M+N), respectively, according to the ordering < defined by the WPQCMP subroutine, and returns in INFO the number of cases where (bw,bp,bq)<(aw,ap,aq), or -i if the i-th parameter had an illegal value.
PURE SUBROUTINE XPQMRG(WPQCMP, M, N, AW, AP, AQ, BW, BP, BQ, XW, XP, XQ, INFO)
  IMPLICIT NONE

  ABSTRACT INTERFACE
     PURE SUBROUTINE PQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
       REAL(KIND=10), INTENT(IN) :: XW, YW
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE PQCMP
  END INTERFACE

  INTEGER, INTENT(IN) :: M, N, AP(M), AQ(M), BP(N), BQ(N)
  REAL(KIND=10), INTENT(IN) :: AW(M), BW(N)
  REAL(KIND=10), INTENT(OUT) :: XW(M+N)
  INTEGER, INTENT(OUT) :: XP(M+N), XQ(M+N), INFO
  PROCEDURE(PQCMP) :: WPQCMP
  INTEGER :: I, J, K, L
  INCLUDE 'gpqmrg.f90'
END SUBROUTINE XPQMRG
