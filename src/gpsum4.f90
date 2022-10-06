  ! This is the generic part of the weight summation routines.
  W = ZERO
  INFO = 0
  IF (.NOT. (W4 .GE. ZERO)) INFO = -4
  IF (.NOT. (W3 .GE. ZERO)) INFO = -3
  IF (.NOT. (W2 .GE. ZERO)) INFO = -2
  IF (.NOT. (W1 .GE. ZERO)) INFO = -1
  IF (INFO .NE. 0) RETURN
  T(1) = W1
  T(2) = W2
  T(3) = W3
  T(4) = W4
  DO I = 3, 1, -1
     INFO = 0
     DO J = 1, I
        K = J + 1
        IF (T(J) .GT. T(K)) THEN
           W = T(J)
           T(J) = T(K)
           T(K) = W
           INFO = INFO + 1
        END IF
     END DO
     IF (INFO .EQ. 0) EXIT
  END DO
  W = T(1)
  DO I = 2, 4
     W = W + T(I)
  END DO
  IF (.NOT. (W .LE. HUGE(W))) THEN
     INFO = -5
  ELSE ! no overflow
     INFO = 0
  END IF
