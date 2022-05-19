  ! This is the generic part of the real Kogbetliantz routines.

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
  IF (.NOT. (B(2,2) .LE. H)) INFO = IERR
  B(1,2) = ABS(G(1,2))
  IF (.NOT. (B(1,2) .LE. H)) INFO = IERR
  B(2,1) = ABS(G(2,1))
  IF (.NOT. (B(2,1) .LE. H)) INFO = IERR
  B(1,1) = ABS(G(1,1))
  IF (.NOT. (B(1,1) .LE. H)) INFO = IERR
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
     INFO = EXPONENT(H) - INFO - 2
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
     ! X = ABS(B(1,1))
     ! Y = ABS(B(2,1))
     ! Z = MAX(X, Y)
     ! IF (Z .EQ. ZERO) THEN
     !    S(1) = ZERO
     ! ELSE ! the general case
     !    X = MIN(X, Y) / Z
     !    Y = X * X + ONE
     !    S(1) = SQRT(Y) * Z
     ! END IF
  END IF

  ! compute the second column norm
  IF (B(1,2) .EQ. ZERO) THEN
     S(2) = ABS(B(2,2))
  ELSE IF (B(2,2) .EQ. ZERO) THEN
     S(2) = ABS(B(1,2))
  ELSE ! full 2nd column
     S(2) = HYPOT(B(1,2), B(2,2))
     ! X = ABS(B(1,2))
     ! Y = ABS(B(2,2))
     ! Z = MAX(X, Y)
     ! IF (Z .EQ. ZERO) THEN
     !    S(2) = ZERO
     ! ELSE ! the general case
     !    X = MIN(X, Y) / Z
     !    Y = X * X + ONE
     !    S(2) = SQRT(Y) * Z
     ! END IF
  END IF

  ! swap the columns if necessary
  IF (S(1) .LT. S(2)) THEN
     Y = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Y

     Y = B(2,1)
     B(2,1) = B(2,2)
     B(2,2) = Y

     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z

     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z

     Y = S(1)
     S(1) = S(2)
     S(2) = Y
  END IF

  ! make B(1,1) non-negative
  IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     U(1,1) = -U(1,1)
     U(1,2) = -U(1,2)
     B(1,1) = -B(1,1)
     B(1,2) = -B(1,2)
  END IF

  ! make B(2,1) non-negative
  IF (SIGN(ONE, B(2,1)) .NE. ONE) THEN
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
     B(2,1) = -B(2,1)
     B(2,2) = -B(2,2)
  END IF

  ! swap the rows if necessary
  IF (B(1,1) .LT. B(2,1)) THEN
     Z = U(1,1)
     U(1,1) = U(2,1)
     U(2,1) = Z

     Z = U(1,2)
     U(1,2) = U(2,2)
     U(2,2) = Z

     X = B(1,1)
     B(1,1) = B(2,1)
     B(2,1) = X

     X = B(1,2)
     B(1,2) = B(2,2)
     B(2,2) = X
  END IF

  ! compute the Givens rotation
  IF (B(1,1) .EQ. ZERO) THEN
     TANG = ZERO
  ELSE ! non-zero B
     TANG = B(2,1) / B(1,1)
  END IF
  SECG = SQRT(TANG * TANG + ONE)

  ! apply the Givens rotation
  B(1,1) = S(1)
  IF (TANG .NE. ZERO) THEN
     B(2,1) = U(1,1)
     U(1,1) = (U(1,1) + TANG * U(2,1)) / SECG
     U(2,1) = (U(2,1) - TANG * B(2,1)) / SECG
     B(2,1) = U(1,2)
     U(1,2) = (U(1,2) + TANG * U(2,2)) / SECG
     U(2,2) = (U(2,2) - TANG * B(2,1)) / SECG
     B(2,1) = B(1,2)
     B(1,2) = (B(1,2) + TANG * B(2,2)) / SECG
     B(2,2) = (B(2,2) - TANG * B(2,1)) / SECG
  END IF
  B(2,1) = ZERO

  ! make B(1,2) non-negative
  IF (SIGN(ONE, B(1,2)) .NE. ONE) THEN
     B(1,2) = -B(1,2)
     B(2,2) = -B(2,2)
     V(1,2) = -V(1,2)
     V(2,2) = -V(2,2)
  END IF

  ! make B(2,2) non-negative
  IF (SIGN(ONE, B(2,2)) .NE. ONE) THEN
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
     B(2,2) = -B(2,2)
  END IF

  ! exit if B is a nul-matrix
  IF (B(1,1) .EQ. ZERO) GOTO 1

  ! divide by B(1,1)
  X = B(1,2) / B(1,1)
  Y = B(2,2) / B(1,1)

  ! the functions of \varphi
  IF (X .LE. Y) THEN
     Z = SCALE(X, 1) * Y
  ELSE ! X > Y
     Z = SCALE(Y, 1) * X
  END IF
  IF (Z .EQ. ZERO) THEN
     TANF = ZERO
     SECF = ONE
  ELSE ! Z > 0
     Z = MIN(Z / ((X - Y) * (X + Y) + ONE), ROOTH)
     TANF = Z / (ONE + SQRT(Z * Z + ONE))
     SECF = SQRT(TANF * TANF + ONE)
  END IF

  ! the functions of \psi
  TANP = Y * TANF + X
  SECP = SQRT(TANP * TANP + ONE)

  ! the scaled singular values
  S(1) = (SECP / SECF) * B(1,1)
  S(2) = (SECF / SECP) * B(2,2)

  ! update U
  Z = U(1,1)
  U(1,1) = (U(1,1) + TANF * U(2,1)) / SECF
  U(2,1) = (U(2,1) - TANF *      Z) / SECF
  Z = U(1,2)
  U(1,2) = (U(1,2) + TANF * U(2,2)) / SECF
  U(2,2) = (U(2,2) - TANF *      Z) / SECF

  ! update V
  Z = V(1,1)
  V(1,1) = (V(1,1) + TANP * V(1,2)) / SECP
  V(1,2) = (V(1,2) - TANP *      Z) / SECP
  Z = V(2,1)
  V(2,1) = (V(2,1) + TANP * V(2,2)) / SECP
  V(2,2) = (V(2,2) - TANP *      Z) / SECP

  ! transpose U
1 Z = U(2,1)
  U(2,1) = U(1,2)
  U(1,2) = Z
