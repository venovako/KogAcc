!>@brief \b ZBRD2 reads a two-dimensional double precision complex array from a binary file.
!!
!!@param U [IN]; a connected unit.
!!@param M [IN]; the number of rows of G.
!!@param N [IN]; the number of columns of G.
!!@param G [OUT]; a double precision complex array to be read.
!!@param LDG [IN]; the leading dimension of G.
!!@param INFO [OUT]; zero on success, -i if the i-th argument had an illegal value, or a positive I/O error code.
SUBROUTINE ZBRD2(U, M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE ZBRD1(U, M, G, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M
       COMPLEX(KIND=REAL64), INTENT(OUT) :: G(M)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE ZBRD1
  END INTERFACE

  INTEGER, INTENT(IN) :: U, M, N, LDG
  COMPLEX(KIND=REAL64), INTENT(OUT) :: G(LDG,N)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: J

  INFO = 0
  IF (LDG .LT. M) INFO = -5
  IF (N .LT. 0) INFO = -3
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN
  IF (LDG .EQ. M) THEN
     READ (UNIT=U, IOSTAT=INFO) G
  ELSE ! LDG .GT. M
     DO J = 1, N
        CALL ZBRD1(U, M, G(1,J), INFO)
        IF (INFO .NE. 0) EXIT
     END DO
  END IF
END SUBROUTINE ZBRD2
