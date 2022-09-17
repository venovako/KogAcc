SUBROUTINE DBWR2(U, M, N, G, LDG, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M, N, LDG
  DOUBLE PRECISION, INTENT(IN) :: G(LDG,N)
  INTEGER, INTENT(OUT) :: INFO
  INTEGER :: J
  EXTERNAL :: DBWR1
  INFO = 0
  IF (LDG .LT. MAX(M, 0)) INFO = -5
  IF (N .LT. 0) INFO = -3
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  IF (N .EQ. 0) RETURN
  DO J = 1, N
     CALL DBWR1(U, M, G(1,J), INFO)
     IF (INFO .NE. 0) EXIT
  END DO
END SUBROUTINE DBWR2
