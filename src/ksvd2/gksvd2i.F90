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
     S(1) = CR_HYPOT(B(1,1), B(2,1))
  END IF

  ! compute the second column norm
  IF (B(1,2) .EQ. ZERO) THEN
     S(2) = ABS(B(2,2))
  ELSE IF (B(2,2) .EQ. ZERO) THEN
     S(2) = ABS(B(1,2))
  ELSE ! full 2nd column
     S(2) = CR_HYPOT(B(1,2), B(2,2))
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
  IF (B(2,1) .EQ. ZERO) THEN
     TANG = ZERO
     SECG = ONE
  ELSE ! B not upper triangular
     TANG = B(2,1) / B(1,1)
#ifdef CR_MATH
     SECG = CR_HYPOT(TANG, ONE)
#else
     SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#endif
  END IF

#ifndef NDEBUG
  WRITE (ERROR_UNIT,2) 'TANG=', TANG, ', SECG=', SECG
#endif

  ! apply the Givens rotation
  B(1,1) = S(1)
  IF (TANG .NE. ZERO) THEN
     X =  TANG
     Y = -TANG
     IF (SECG .NE. ONE) THEN
        B(2,1) = U(1,1)
        U(1,1) = IEEE_FMA(X, U(2,1), U(1,1)) / SECG
        U(2,1) = IEEE_FMA(Y, B(2,1), U(2,1)) / SECG
        B(2,1) = U(1,2)
        U(1,2) = IEEE_FMA(X, U(2,2), U(1,2)) / SECG
        U(2,2) = IEEE_FMA(Y, B(2,1), U(2,2)) / SECG
        B(2,1) = B(1,2)
        B(1,2) = IEEE_FMA(X, B(2,2), B(1,2)) / SECG
        B(2,2) = IEEE_FMA(Y, B(2,1), B(2,2)) / SECG
     ELSE ! SECG = 1
        B(2,1) = U(1,1)
        U(1,1) = IEEE_FMA(X, U(2,1), U(1,1))
        U(2,1) = IEEE_FMA(Y, B(2,1), U(2,1))
        B(2,1) = U(1,2)
        U(1,2) = IEEE_FMA(X, U(2,2), U(1,2))
        U(2,2) = IEEE_FMA(Y, B(2,1), U(2,2))
        B(2,1) = B(1,2)
        B(1,2) = IEEE_FMA(X, B(2,2), B(1,2))
        B(2,2) = IEEE_FMA(Y, B(2,1), B(2,2))
     END IF
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

  ! exit if B is diagonal
  IF (B(1,2) .EQ. ZERO) GOTO 1

  ! divide by B(1,1)
  X = B(1,2) / B(1,1)
  Y = B(2,2) / B(1,1)

#ifndef NDEBUG
  WRITE (ERROR_UNIT,2) '   X=', X, ',    Y=', Y
#endif
  IF (X .EQ. ZERO) GOTO 1

  ! a partial fix
  IF (Y .EQ. ONE) THEN
     Z = TWO / X
  ELSE IF (X .EQ. ONE) THEN
     Z = SCALE(Y, 1)
     Z = Z / IEEE_FMA(-Y, Y, TWO)
  ELSE IF (X .EQ. Y) THEN
     Z = SCALE(X, 1) * Y
  ELSE IF (X .LT. Y) THEN
     Z = SCALE(X, 1) * Y
     Z = Z / IEEE_FMA(X, X, IEEE_FMA(-Y, Y, ONE))
  ELSE ! X > Y
     Z = SCALE(Y, 1) * X
     Z = Z / IEEE_FMA(X - Y, X + Y, ONE)
  END IF

  ! the functions of \varphi
  IF (Z .EQ. ZERO) THEN
     TANF = ZERO
     SECF = ONE
  ELSE ! Z <> 0
     Z = SIGN(MIN(ABS(Z), ROOTH), Z)
#ifdef CR_MATH
     TANF = CR_HYPOT(Z, ONE)
#else
     TANF = SQRT(IEEE_FMA(Z, Z, ONE))
#endif
     TANF = Z / (ONE + TANF)
#ifdef CR_MATH
     SECF = CR_HYPOT(TANF, ONE)
#else
     SECF = SQRT(IEEE_FMA(TANF, TANF, ONE))
#endif
  END IF

#ifndef NDEBUG
  WRITE (ERROR_UNIT,2) 'TANF=', TANF, ', SECF=', SECF
#endif

  ! the functions of \psi
  TANP = IEEE_FMA(Y, TANF, X)
#ifdef CR_MATH
  SECP = CR_HYPOT(TANP, ONE)
#else
  SECP = SQRT(IEEE_FMA(TANP, TANP, ONE))
#endif

#ifndef NDEBUG
  WRITE (ERROR_UNIT,2) 'TANP=', TANP, ', SECP=', SECP
#endif

  ! update U
  X =  TANF
  Y = -TANF
  IF (SECF .NE. ONE) THEN
     S(1) = (SECP / SECF) * B(1,1) ! the first scaled singular value
     Z = U(1,1)
     U(1,1) = IEEE_FMA(X, U(2,1), U(1,1)) / SECF
     U(2,1) = IEEE_FMA(Y,      Z, U(2,1)) / SECF
     Z = U(1,2)
     U(1,2) = IEEE_FMA(X, U(2,2), U(1,2)) / SECF
     U(2,2) = IEEE_FMA(Y,      Z, U(2,2)) / SECF
  ELSE ! SECF = 1
     S(1) = SECP * B(1,1) ! the first scaled singular value
     Z = U(1,1)
     U(1,1) = IEEE_FMA(X, U(2,1), U(1,1))
     U(2,1) = IEEE_FMA(Y,      Z, U(2,1))
     Z = U(1,2)
     U(1,2) = IEEE_FMA(X, U(2,2), U(1,2))
     U(2,2) = IEEE_FMA(Y,      Z, U(2,2))
  END IF

  ! update V
  X =  TANP
  Y = -TANP
  IF (SECP .NE. ONE) THEN
     S(2) = (SECF / SECP) * B(2,2) ! the second scaled singular value
     Z = V(1,1)
     V(1,1) = IEEE_FMA(X, V(1,2), V(1,1)) / SECP
     V(1,2) = IEEE_FMA(Y,      Z, V(1,2)) / SECP
     Z = V(2,1)
     V(2,1) = IEEE_FMA(X, V(2,2), V(2,1)) / SECP
     V(2,2) = IEEE_FMA(Y,      Z, V(2,2)) / SECP
  ELSE ! SECP = 1
     S(2) = SECF * B(2,2) ! the second scaled singular value
     Z = V(1,1)
     V(1,1) = IEEE_FMA(X, V(1,2), V(1,1))
     V(1,2) = IEEE_FMA(Y,      Z, V(1,2))
     Z = V(2,1)
     V(2,1) = IEEE_FMA(X, V(2,2), V(2,1))
     V(2,2) = IEEE_FMA(Y,      Z, V(2,2))
  END IF

  ! symmetric permutation if S(1) < S(2)
1 IF (S(1) .LT. S(2)) THEN
     Z = U(1,1)
     U(1,1) = U(2,1)
     U(2,1) = Z
     Z = U(1,2)
     U(1,2) = U(2,2)
     U(2,2) = Z
     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z
     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z
     Z = S(1)
     S(1) = S(2)
     S(2) = Z
  END IF

  ! transpose U
  Z = U(2,1)
  U(2,1) = U(1,2)
  U(1,2) = Z
