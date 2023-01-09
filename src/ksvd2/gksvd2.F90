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

  ! swap the columns if necessary, avoiding the QR (especially with a small angle) if possible
  IF ((S(1) .LT. S(2)) .OR. ((S(1) .EQ. S(2)) .AND. (B(1,1) .NE. ZERO) .AND. (B(2,1) .NE. ZERO) .AND. &
       ((B(1,2) .EQ. ZERO) .OR. (B(2,2) .EQ. ZERO) .OR. ((ABS(B(1,1)) + ABS(B(2,1))) .LT. (ABS(B(1,2)) + ABS(B(2,2))))))) THEN
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

  ! swap the rows if necessary
  IF (ABS(B(1,1)) .LT. ABS(B(2,1))) THEN
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

  ! make B(1,1) non-negative
  IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     IF (U(1,1) .EQ. ZERO) THEN
        U(1,1) = ZERO
     ELSE ! change the sign
        U(1,1) = -U(1,1)
     END IF
     IF (U(1,2) .EQ. ZERO) THEN
        U(1,2) = ZERO
     ELSE ! change the sign
        U(1,2) = -U(1,2)
     END IF
     B(1,1) = -B(1,1)
     IF (B(1,2) .EQ. ZERO) THEN
        B(1,2) = ZERO
     ELSE ! change the sign
        B(1,2) = -B(1,2)
     END IF
  END IF

  ! make B(2,1) non-negative
  IF (B(2,1) .EQ. ZERO) THEN
     B(2,1) = ZERO
  ELSE IF (B(2,1) .LT. ZERO) THEN
     IF (U(2,1) .EQ. ZERO) THEN
        U(2,1) = ZERO
     ELSE ! change the sign
        U(2,1) = -U(2,1)
     END IF
     IF (U(2,2) .EQ. ZERO) THEN
        U(2,2) = ZERO
     ELSE ! change the sign
        U(2,2) = -U(2,2)
     END IF
     B(2,1) = -B(2,1)
     B(2,2) = -B(2,2)
  END IF

  ! compute the Givens rotation
  IF (B(2,1) .EQ. ZERO) THEN
     TANG = ZERO
     SECG = ONE
  ELSE ! B not upper triangular
     TANG = B(2,1) / B(1,1)
     IF (TANG .GT. ZERO) THEN
#ifdef CR_MATH
        SECG = CR_HYPOT(TANG, ONE)
#else
        SECG = SQRT(TANG * TANG + ONE)
#endif
     ELSE ! TANG = 0
        SECG = 1
     END IF
  END IF
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANG=', TANG, ', SECG=', SECG
#endif

  ! apply the Givens rotation
  B(1,1) = S(1)
  IF (TANG .GT. ZERO) THEN
     IF (SECG .GT. ONE) THEN
        B(2,1) = U(1,1)
        U(1,1) = (U(1,1) + TANG * U(2,1)) / SECG
        U(2,1) = (U(2,1) - TANG * B(2,1)) / SECG
        B(2,1) = U(1,2)
        U(1,2) = (U(1,2) + TANG * U(2,2)) / SECG
        U(2,2) = (U(2,2) - TANG * B(2,1)) / SECG
        B(2,1) = B(1,2)
        B(1,2) = (B(1,2) + TANG * B(2,2)) / SECG
        B(2,2) = (B(2,2) - TANG * B(2,1)) / SECG
     ELSE ! SECG = 1
        B(2,1) = U(1,1)
        U(1,1) = U(1,1) + TANG * U(2,1)
        U(2,1) = U(2,1) - TANG * B(2,1)
        B(2,1) = U(1,2)
        U(1,2) = U(1,2) + TANG * U(2,2)
        U(2,2) = U(2,2) - TANG * B(2,1)
        B(2,1) = B(1,2)
        B(1,2) = B(1,2) + TANG * B(2,2)
        B(2,2) = B(2,2) - TANG * B(2,1)
     END IF
  END IF
  B(2,1) = ZERO

  ! make B(1,2) non-negative
  IF (B(1,2) .EQ. ZERO) THEN
     B(1,2) = ZERO
  ELSE IF (B(1,2) .LT. ZERO) THEN
     B(1,2) = -B(1,2)
     B(2,2) = -B(2,2)
     IF (V(1,2) .EQ. ZERO) THEN
        V(1,2) = ZERO
     ELSE ! change the sign
        V(1,2) = -V(1,2)
     END IF
     IF (V(2,2) .EQ. ZERO) THEN
        V(2,2) = ZERO
     ELSE ! change the sign
        V(2,2) = -V(2,2)
     END IF
  END IF

  ! make B(2,2) non-negative
  IF (SIGN(ONE, B(2,2)) .NE. ONE) THEN
     IF (U(2,1) .EQ. ZERO) THEN
        U(2,1) = ZERO
     ELSE ! change the sign
        U(2,1) = -U(2,1)
     END IF
     IF (U(2,2) .EQ. ZERO) THEN
        U(2,2) = ZERO
     ELSE ! change the sign
        U(2,2) = -U(2,2)
     END IF
     B(2,2) = -B(2,2)
  END IF

  ! exit if B is diagonal
  IF (B(1,2) .EQ. ZERO) GOTO 8

  ! divide by B(1,1)
  ! [ 1 x ]
  ! [ 0 y ]
  X = B(1,2) / B(1,1)
  Y = B(2,2) / B(1,1)
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) '   X=', X, ',    Y=', Y
#endif
  IF (X .EQ. ZERO) GOTO 8

  ! a partial fix
  IF (Y .EQ. ONE) THEN
     Z = TWO / X
  ELSE IF (X .EQ. ONE) THEN
     Z = SCALE(Y, 1)
     Z = Z / (TWO - Y * Y)
  ELSE IF (X .EQ. Y) THEN
     Z = SCALE(X, 1) * Y
  ELSE IF (X .LT. Y) THEN
     Z = SCALE(X, 1) * Y
     Z = Z / ((ONE - Y * Y) + X * X)
  ELSE ! X > Y
     Z = SCALE(Y, 1) * X
     Z = Z / ((X - Y) * (X + Y) + ONE)
  END IF
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) '   Z=', Z, ',ROOTH=', ROOTH
#endif

  ! the functions of \varphi
  ! Negative Z can happen only if Y > 1 (impossible mathematically),
  ! what in turn, with the correctly rounded hypot, can happen only
  ! as a consequence of the QR factorization.
  IF (Z .EQ. ZERO) THEN
     TANF = ZERO
     SECF = ONE
  ELSE IF (ABS(Z) .GT. HUGE(Z)) THEN
     TANF = SIGN(ONE, Z)
     SECF = SQRT(TWO)
  ELSE ! finite non-zero Z
#ifdef CR_MATH
     TANF = CR_HYPOT(Z, ONE)
#else
     Z = SIGN(MIN(ABS(Z), ROOTH), Z)
     TANF = SQRT(Z * Z + ONE)
#endif
     TANF = Z / (ONE + TANF)
#ifdef CR_MATH
     SECF = CR_HYPOT(TANF, ONE)
#else
     SECF = SQRT(TANF * TANF + ONE)
#endif
  END IF
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANF=', TANF, ', SECF=', SECF
#endif

  ! the functions of \psi
  TANP = Y * TANF + X
#ifdef CR_MATH
  SECP = CR_HYPOT(TANP, ONE)
#else
  SECP = SQRT(TANP * TANP + ONE)
#endif
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANP=', TANP, ', SECP=', SECP
#endif

  ! update U
  IF (SECF .NE. ONE) THEN
     S(1) = (SECP / SECF) * B(1,1) ! the first scaled singular value
     Z = U(1,1)
     U(1,1) = (U(1,1) + TANF * U(2,1)) / SECF
     U(2,1) = (U(2,1) - TANF *      Z) / SECF
     Z = U(1,2)
     U(1,2) = (U(1,2) + TANF * U(2,2)) / SECF
     U(2,2) = (U(2,2) - TANF *      Z) / SECF
  ELSE ! SECF = 1
     S(1) = SECP * B(1,1) ! the first scaled singular value
     Z = U(1,1)
     U(1,1) = U(1,1) + TANF * U(2,1)
     U(2,1) = U(2,1) - TANF *      Z
     Z = U(1,2)
     U(1,2) = U(1,2) + TANF * U(2,2)
     U(2,2) = U(2,2) - TANF *      Z
  END IF

  ! update V
  IF (SECP .NE. ONE) THEN
     S(2) = (SECF / SECP) * B(2,2) ! the second scaled singular value
     Z = V(1,1)
     V(1,1) = (V(1,1) + TANP * V(1,2)) / SECP
     V(1,2) = (V(1,2) - TANP *      Z) / SECP
     Z = V(2,1)
     V(2,1) = (V(2,1) + TANP * V(2,2)) / SECP
     V(2,2) = (V(2,2) - TANP *      Z) / SECP
  ELSE ! SECP = 1
     S(2) = SECF * B(2,2) ! the second scaled singular value
     Z = V(1,1)
     V(1,1) = V(1,1) + TANP * V(1,2)
     V(1,2) = V(1,2) - TANP *      Z
     Z = V(2,1)
     V(2,1) = V(2,1) + TANP * V(2,2)
     V(2,2) = V(2,2) - TANP *      Z
  END IF

  ! symmetric permutation if S(1) < S(2)
8 IF (S(1) .LT. S(2)) THEN
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
     Y = S(1)
     S(1) = S(2)
     S(2) = Y
  END IF

  ! transpose U
  IF (U(1,1) .EQ. ZERO) U(1,1) = ZERO
  IF (U(2,1) .EQ. ZERO) U(2,1) = ZERO
  IF (U(1,2) .EQ. ZERO) U(1,2) = ZERO
  IF (U(2,2) .EQ. ZERO) U(2,2) = ZERO
  Z = U(2,1)
  U(2,1) = U(1,2)
  U(1,2) = Z
