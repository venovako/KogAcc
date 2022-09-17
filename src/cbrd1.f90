SUBROUTINE CBRD1(U, M, G, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: U, M
  COMPLEX, INTENT(OUT) :: G(M)
  INTEGER, INTENT(OUT) :: INFO
  INFO = 0
  IF (M .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (M .EQ. 0) RETURN
  READ (UNIT=U, IOSTAT=INFO) G
END SUBROUTINE CBRD1
