!>@brief \b SPQMRG merges the arrays \f$AW,AP,AQ\f$ (of length \f$M\f$) and \f$BW,BP,BQ\f$ (of length \f$N\f$) into the arrays \f$XW,XP,XQ\f$ (of length \f$M+N\f$), respectively, according to the ordering \f$\prec\f$ defined by the WPQCMP subroutine, and returns in INFO the number of cases where \f$(bw,bp,bq)\prec(aw,ap,aq)\f$, or \f$-i\f$ if the \f$i\f$th parameter had an illegal value.
SUBROUTINE SPQMRG(WPQCMP, M, N, AW, AP, AQ, BW, BP, BQ, XW, XP, XQ, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M, N, AP(M), AQ(M), BP(N), BQ(N)
  REAL(KIND=REAL32), INTENT(IN) :: AW(M), BW(N)
  REAL(KIND=REAL32), INTENT(OUT) :: XW(M+N)
  INTEGER, INTENT(OUT) :: XP(M+N), XQ(M+N), INFO
  EXTERNAL :: WPQCMP
  INTEGER :: I, J, K, L
  INCLUDE 'gpqmrg.f90'
END SUBROUTINE SPQMRG
