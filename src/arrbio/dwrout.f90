!>@brief \b DWROUT writes the BN.UJ, BN.VJ, and BN.SJ files.
SUBROUTINE DWROUT(BN, J, N, U, LDU, V, LDV, S, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, REAL128
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
     SUBROUTINE DBWR2(U, M, N, G, LDG, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M, N, LDG
       REAL(KIND=REAL64), INTENT(IN) :: G(LDG,N)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DBWR2
  END INTERFACE
  INTERFACE
     SUBROUTINE QBWR1(U, M, G, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: U, M
       REAL(KIND=REAL128), INTENT(IN) :: G(M)
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE QBWR1
  END INTERFACE

  CHARACTER(LEN=2), PARAMETER :: ACT = 'WO'
  CHARACTER(LEN=*), INTENT(IN) :: BN
  INTEGER, INTENT(IN) :: J, N, LDU, LDV
  REAL(KIND=REAL64), INTENT(IN) :: U(LDU,N), V(LDV,N)
  REAL(KIND=REAL128), INTENT(IN) :: S(N)
  INTEGER, INTENT(OUT) :: INFO
  CHARACTER :: CJ
  INTEGER :: I
  
  INFO = 0
  IF (LDV .LT. N) INFO = -7
  IF (LDU .LT. N) INFO = -5
  IF (N .LT. 0) INFO = -3
  IF ((J .LT. 0) .OR. (J .GT. 4)) INFO = -2
  IF (LEN_TRIM(BN) .LE. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN

  SELECT CASE (J)
  CASE (0)
     CJ = '0'
  CASE (1)
     CJ = '1'
  CASE (2)
     CJ = '2'
  CASE (3)
     CJ = '3'
  CASE (4)
     CJ = '4'
  CASE DEFAULT
     INFO = -9
     RETURN
  END SELECT

  CALL BFOPEN(TRIM(BN)//'.U'//CJ, ACT, I, INFO)
  IF (INFO .NE. 0) RETURN
  CALL DBWR2(I, N, N, U, LDU, INFO)
  IF (INFO .NE. 0) RETURN
  CLOSE(I, IOSTAT=INFO)
  IF (INFO .NE. 0) RETURN

  CALL BFOPEN(TRIM(BN)//'.V'//CJ, ACT, I, INFO)
  IF (INFO .NE. 0) RETURN
  CALL DBWR2(I, N, N, V, LDV, INFO)
  IF (INFO .NE. 0) RETURN
  CLOSE(I, IOSTAT=INFO)
  IF (INFO .NE. 0) RETURN

  CALL BFOPEN(TRIM(BN)//'.S'//CJ, ACT, I, INFO)
  IF (INFO .NE. 0) RETURN
  CALL QBWR1(I, N, S, INFO)
  IF (INFO .NE. 0) RETURN
  CLOSE(I, IOSTAT=INFO)
  IF (INFO .NE. 0) RETURN
END SUBROUTINE DWROUT
