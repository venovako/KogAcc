  ! U = I
  U(1,1) = ONE
  U(2,1) = ZERO
  U(1,2) = ZERO
  U(2,2) = ONE
  ! V = I
  V(1,1) = ONE
  V(2,1) = ZERO
  V(1,2) = ZERO
  V(2,2) = ONE
  ! S = 0
  S(1) = ZERO
  S(2) = ZERO
  INFO = 0

  ! check if G has a non-finite value
  B(2,2) = ABS(G(2,2))
  IF (.NOT. (B(2,2) .LE. HUGE(ZERO))) INFO = IERR
  B(1,2) = ABS(G(1,2))
  IF (.NOT. (B(1,2) .LE. HUGE(ZERO))) INFO = IERR
  B(2,1) = ABS(G(2,1))
  IF (.NOT. (B(2,1) .LE. HUGE(ZERO))) INFO = IERR
  B(1,1) = ABS(G(1,1))
  IF (.NOT. (B(1,1) .LE. HUGE(ZERO))) INFO = IERR
  IF (INFO .NE. 0) RETURN

  ! determine the scaling factor s
  INFO = IERR
  IF (B(1,1) .NE. ZERO) INFO = MAX(INFO, EXPONENT(B(1,1)))
  IF (B(2,1) .NE. ZERO) INFO = MAX(INFO, EXPONENT(B(2,1)))
  IF (B(1,2) .NE. ZERO) INFO = MAX(INFO, EXPONENT(B(1,2)))
  IF (B(2,2) .NE. ZERO) INFO = MAX(INFO, EXPONENT(B(2,2)))
  IF (INFO .EQ. IERR) THEN
     INFO = 0
  ELSE ! non-zero B
     INFO = EXPONENT(HUGE(ZERO)) - INFO - 2
  END IF

  ! scale G
  IF (INFO .NE. 0) THEN
     B(1,1) = SCALE(G(1,1), INFO)
     B(2,1) = SCALE(G(2,1), INFO)
     B(1,2) = SCALE(G(1,2), INFO)
     B(2,2) = SCALE(G(2,2), INFO)
  ELSE ! no scaling
     B(1,1) = G(1,1)
     B(2,1) = G(2,1)
     B(1,2) = G(1,2)
     B(2,2) = G(2,2)
  END IF

  ! compute the first column norm
  IF (B(2,1) .EQ. ZERO) THEN
     S(1) = ABS(B(1,1))
  ELSE IF (B(1,1) .EQ. ZERO) THEN
     S(1) = ABS(B(2,1))
  ELSE ! full 1st column
     S(1) = HYPOT(B(1,1), B(2,1))
  END IF

  ! compute the second column norm
  IF (B(1,2) .EQ. ZERO) THEN
     S(2) = ABS(B(2,2))
  ELSE IF (B(2,2) .EQ. ZERO) THEN
     S(2) = ABS(B(1,2))
  ELSE ! full 2nd column
     S(2) = HYPOT(B(1,2), B(2,2))
  END IF

  ! swap the columns if necessary
  IF (S(1) .LT. S(2)) THEN
     X = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = X

     X = B(2,1)
     B(2,1) = B(2,2)
     B(2,2) = X

     X = S(1)
     S(1) = S(2)
     S(2) = X

     X = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = X

     X = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = X
  END IF

  ! make B(1,1) non-negative
  IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     B(1,1) = -B(1,1)
     B(1,2) = -B(1,2)
     U(1,1) = -U(1,1)
     U(1,2) = -U(1,2)
  END IF

  ! make B(2,1) non-negative
  IF (SIGN(ONE, B(2,1)) .NE. ONE) THEN
     B(2,1) = -B(2,1)
     B(2,2) = -B(2,2)
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
  END IF

  ! swap the rows if necessary
  IF (B(1,1) .LT. B(2,1)) THEN
     X = B(1,1)
     B(1,1) = B(2,1)
     B(2,1) = X

     X = U(1,1)
     U(1,1) = U(2,1)
     U(2,1) = X

     X = B(1,2)
     B(1,2) = B(2,2)
     B(2,2) = X

     X = U(1,2)
     U(1,2) = U(2,2)
     U(2,2) = X
  END IF

  ! compute the Givens rotation
  IF (B(1,1) .EQ. ZERO) THEN
     TANG = ZERO
  ELSE ! non-zero B
     TANG = B(2,1) / B(1,1)
  END IF
  SECG = SQRT(ONE + TANG * TANG)

  ! apply the Givens rotation
  B(1,1) = S(1)
  B(2,1) = ZERO
  B(1,2) = (B(1,2) + TANG * B(2,2)) / SECG
  B(2,2) = (B(2,2) - TANG * B(1,2)) / SECG
  U(1,1) = (U(1,1) + TANG * U(2,1)) / SECG
  U(2,1) = (U(2,1) - TANG * U(1,1)) / SECG
  U(1,2) = (U(1,2) + TANG * U(2,2)) / SECG
  U(2,2) = (U(2,2) - TANG * U(1,2)) / SECG

  ! make B(1,2) non-negative
  IF (SIGN(ONE, B(1,2)) .NE. ONE) THEN
     B(1,2) = -B(1,2)
     B(2,2) = -B(2,2)
     V(1,2) = -V(1,2)
     V(2,2) = -V(2,2)
  END IF

  ! make B(2,2) non-negative
  IF (SIGN(ONE, B(2,2)) .NE. ONE) THEN
     B(2,1) = -B(2,1)
     B(2,2) = -B(2,2)
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
  END IF

  ! TODO

  ! transpose U
  X = U(2,1)
  U(2,1) = U(1,2)
  U(1,2) = X
