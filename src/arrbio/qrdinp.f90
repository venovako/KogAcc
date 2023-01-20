!>@brief \b QRDINP reads the BN.G file with an NxN matrix into the MxM matrix G.
SUBROUTINE QRDINP(BN, M, N, G, LDG, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE BFOPEN(FN, ACT, U, INFO)
       IMPLICIT NONE
       CHARACTER(LEN=*), INTENT(IN) :: FN
       CHARACTER(LEN=2), INTENT(IN) :: ACT
       INTEGER, INTENT(OUT) :: U, INFO
     END SUBROUTINE BFOPEN
  END INTERFACE
  INTERFACE
     SUBROUTINE QBRD2(U, M, N, G, LDG, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M, N, LDG
       REAL(KIND=REAL128), INTENT(OUT) :: G(LDG,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE QBRD2
  END INTERFACE

  CHARACTER(LEN=2), PARAMETER :: ACT = 'RO'
  CHARACTER(LEN=*), INTENT(IN) :: BN
  INTEGER, INTENT(IN) :: M, N, LDG
  REAL(KIND=REAL128), INTENT(OUT) :: G(LDG,M)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: I
  
  INFO = 0
  IF (LDG .LT. M) INFO = -5
  IF ((N .LT. 0) .OR. (N .GT. M)) INFO = -3
  IF (M .LT. 0) INFO = -2
  IF (LEN_TRIM(BN) .LE. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  CALL BFOPEN(TRIM(BN)//'.G', ACT, I, INFO)
  IF (INFO .NE. 0) RETURN
  CALL QBRD2(I, N, N, G, LDG, INFO)
  IF (INFO .NE. 0) RETURN
  CLOSE(I, IOSTAT=INFO)
END SUBROUTINE QRDINP
