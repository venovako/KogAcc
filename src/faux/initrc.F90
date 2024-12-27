!>@brief \b INITRC initializes O with the row-cyclic ordering.
PURE SUBROUTINE INITRC(N, O, INFO)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: N
  INTEGER, INTENT(OUT) :: O(2,*), INFO

  INTEGER :: I, J, K

  INFO = 0
  IF (N .LE. 1) INFO = -1
  IF (INFO .NE. 0) RETURN

  K = 0
  DO I = 1, N-1
     DO J = I+1, N
        K = K + 1
        O(1,K) = I
        O(2,K) = J
     END DO
  END DO
  INFO = K
END SUBROUTINE INITRC
