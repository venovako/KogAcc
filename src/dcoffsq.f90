SUBROUTINE DCOFFSQ(O, N, A, LDA, D, INFO)
  IMPLICIT NONE
  CHARACTER, INTENT(IN) :: O
  INTEGER, INTENT(IN) :: N, LDA
  COMPLEX, INTENT(IN) :: A(LDA,N)
  DOUBLE PRECISION, INTENT(OUT) :: D
  INTEGER, INTENT(OUT) :: INFO
  DOUBLE PRECISION :: Y
  INTEGER :: I, J
  D = 0.0D0
  INFO = 0
  IF (LDA .LT. N) INFO = -4
  IF (N .LT. 0) INFO = -2
  IF (INFO .NE. 0) RETURN
  IF (N .EQ. 0) RETURN
  SELECT CASE (O)
  CASE('A')
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,A) PRIVATE(I,Y) REDUCTION(+:D)
     DO J = 1, N
        DO I = 1, N
           Y = DBLE(REAL(A(I,J)))
           D = D + Y * Y
           Y = DBLE(AIMAG(A(I,J)))
           D = D + Y * Y
        END DO
     END DO
     !$OMP END PARALLEL DO
  CASE('O')
     !$OMP PARALLEL DO DEFAULT(NONE) SHARED(N,A) PRIVATE(I,Y) REDUCTION(+:D)
     DO J = 1, N
        DO I = 1, J-1
           Y = DBLE(REAL(A(I,J)))
           D = D + Y * Y
           Y = DBLE(AIMAG(A(I,J)))
           D = D + Y * Y
        END DO
        DO I = J+1, N
           Y = DBLE(REAL(A(I,J)))
           D = D + Y * Y
           Y = DBLE(AIMAG(A(I,J)))
           D = D + Y * Y
        END DO
     END DO
     !$OMP END PARALLEL DO
  CASE('a')
     DO J = 1, N
        DO I = 1, N
           Y = DBLE(REAL(A(I,J)))
           D = D + Y * Y
           Y = DBLE(AIMAG(A(I,J)))
           D = D + Y * Y
        END DO
     END DO
  CASE('o')
     DO J = 1, N
        DO I = 1, J-1
           Y = DBLE(REAL(A(I,J)))
           D = D + Y * Y
           Y = DBLE(AIMAG(A(I,J)))
           D = D + Y * Y
        END DO
        DO I = J+1, N
           Y = DBLE(REAL(A(I,J)))
           D = D + Y * Y
           Y = DBLE(AIMAG(A(I,J)))
           D = D + Y * Y
        END DO
     END DO
  CASE DEFAULT
     INFO = -1
  END SELECT
END SUBROUTINE DCOFFSQ
