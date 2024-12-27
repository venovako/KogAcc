!>@brief \b INITCC initializes O with the column-cyclic ordering.
PURE SUBROUTINE INITCC(N, O, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  INTEGER, INTENT(OUT) :: O(2,*), INFO

  INTEGER :: I, J, K

  INFO = 0
  IF (N .LE. 1) INFO = -1
  IF (INFO .NE. 0) RETURN

  K = 1
  DO J = 2, N
     DO I = 1, J-1
        O(1,K) = I
        O(2,K) = J
        K = K + 1
     END DO
  END DO
END SUBROUTINE INITCC
