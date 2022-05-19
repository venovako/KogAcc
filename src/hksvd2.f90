  ! This is the generic part of the complex Kogbetliantz routines.

  ! U = I
  U(1,1) = CONE
  U(2,1) = CZERO
  U(1,2) = CZERO
  U(2,2) = CONE
  ! V = I
  V(1,1) = CONE
  V(2,1) = CZERO
  V(1,2) = CZERO
  V(2,2) = CONE
  ! S = 0
  S(1) = ZERO
  S(2) = ZERO
  INFO = 0

  ! check if G has a non-finite value
  Y = ABS(AIMAG(G(2,2)))
  IF (.NOT. (Y .LE. H)) INFO = IERR
  X = ABS(REAL(G(2,2)))
  IF (.NOT. (X .LE. H)) INFO = IERR
  B(2,2) = CMPLX(X, Y, K)
  Y = ABS(AIMAG(G(1,2)))
  IF (.NOT. (Y .LE. H)) INFO = IERR
  X = ABS(REAL(G(1,2)))
  IF (.NOT. (X .LE. H)) INFO = IERR
  B(1,2) = CMPLX(X, Y, K)
  Y = ABS(AIMAG(G(2,1)))
  IF (.NOT. (Y .LE. H)) INFO = IERR
  X = ABS(REAL(G(2,1)))
  IF (.NOT. (X .LE. H)) INFO = IERR
  B(2,1) = CMPLX(X, Y, K)
  Y = ABS(AIMAG(G(1,1)))
  IF (.NOT. (Y .LE. H)) INFO = IERR
  X = ABS(REAL(G(1,1)))
  IF (.NOT. (X .LE. H)) INFO = IERR
  B(1,1) = CMPLX(X, Y, K)
  IF (INFO .NE. 0) RETURN

  ! determine the scaling factor s
  INFO = IERR
  ! ... real parts ...
  X = REAL(B(1,1))
  IF (X .NE. ZERO) INFO = MAX(INFO, EXPONENT(X))
  X = REAL(B(2,1))
  IF (X .NE. ZERO) INFO = MAX(INFO, EXPONENT(X))
  X = REAL(B(1,2))
  IF (X .NE. ZERO) INFO = MAX(INFO, EXPONENT(X))
  X = REAL(B(2,2))
  IF (X .NE. ZERO) INFO = MAX(INFO, EXPONENT(X))
  ! ... imaginary parts ...
  Y = AIMAG(B(1,1))
  IF (Y .NE. ZERO) INFO = MAX(INFO, EXPONENT(Y))
  Y = AIMAG(B(2,1))
  IF (Y .NE. ZERO) INFO = MAX(INFO, EXPONENT(Y))
  Y = AIMAG(B(1,2))
  IF (Y .NE. ZERO) INFO = MAX(INFO, EXPONENT(Y))
  Y = AIMAG(B(2,2))
  IF (Y .NE. ZERO) INFO = MAX(INFO, EXPONENT(Y))
  ! compute s
  IF (INFO .EQ. IERR) THEN
     INFO = 0
  ELSE ! non-zero G
     INFO = EXPONENT(H) - INFO - 2
  END IF

  ! scale G
  IF (INFO .NE. 0) THEN
     B(1,1) = CMPLX(SCALE(REAL(G(1,1)), INFO), SCALE(AIMAG(G(1,1)), INFO), K)
     B(2,1) = CMPLX(SCALE(REAL(G(2,1)), INFO), SCALE(AIMAG(G(2,1)), INFO), K)
     B(1,2) = CMPLX(SCALE(REAL(G(1,2)), INFO), SCALE(AIMAG(G(1,2)), INFO), K)
     B(2,2) = CMPLX(SCALE(REAL(G(2,2)), INFO), SCALE(AIMAG(G(2,2)), INFO), K)
  ELSE ! no scaling
     B(1,1) = CMPLX(REAL(G(1,1)), AIMAG(G(1,1)), K)
     B(2,1) = CMPLX(REAL(G(2,1)), AIMAG(G(2,1)), K)
     B(1,2) = CMPLX(REAL(G(1,2)), AIMAG(G(1,2)), K)
     B(2,2) = CMPLX(REAL(G(2,2)), AIMAG(G(2,2)), K)
  END IF

  ! compute the absolute values
  A(1,1) = ABS(B(1,1))
  A(2,1) = ABS(B(2,1))
  A(1,2) = ABS(B(1,2))
  A(2,2) = ABS(B(2,2))

  ! compute the first column norm
  IF (A(2,1) .EQ. ZERO) THEN
     S(1) = A(1,1)
  ELSE IF (A(1,1) .EQ. ZERO) THEN
     S(1) = A(2,1)
  ELSE ! full 1st column
     S(1) = HYPOT(A(1,1), A(2,1))
  END IF

  ! compute the second column norm
  IF (A(1,2) .EQ. ZERO) THEN
     S(2) = A(2,2)
  ELSE IF (A(2,2) .EQ. ZERO) THEN
     S(2) = A(1,2)
  ELSE ! full 2nd column
     S(2) = HYPOT(A(1,2), A(2,2))
  END IF

  ! swap the columns if necessary
  IF (S(1) .LT. S(2)) THEN
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z

     Z = B(2,1)
     B(2,1) = B(2,2)
     B(2,2) = Z

     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z

     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z

     Y = A(1,1)
     A(1,1) = A(1,2)
     A(1,2) = Y

     Y = A(2,1)
     A(2,1) = A(2,2)
     A(2,2) = Y

     Y = S(1)
     S(1) = S(2)
     S(2) = Y
  END IF

  ! make B(1,1) real and non-negative
  IF (AIMAG(B(1,1)) .EQ. ZERO) THEN
     IF (SIGN(ONE, REAL(B(1,1))) .NE. ONE) THEN
        U(1,1) = -U(1,1)
        U(1,2) = -U(1,2)
        B(1,1) = -B(1,1)
        B(1,2) = -B(1,2)
     END IF
  ELSE ! the general case
     Z = CONJG(B(1,1)) / A(1,1)
     U(1,1) = Z
     B(1,1) = A(1,1)
     B(1,2) = Z * B(1,2)
  END IF

  ! make B(2,1) real and non-negative
  IF (AIMAG(B(2,1)) .EQ. ZERO) THEN
     IF (SIGN(ONE, REAL(B(2,1))) .NE. ONE) THEN
        U(2,1) = -U(2,1)
        U(2,2) = -U(2,2)
        B(2,1) = -B(2,1)
        B(2,2) = -B(2,2)
     END IF
  ELSE ! the general case
     Z = CONJG(B(2,1)) / A(2,1)
     U(2,2) = Z
     B(2,1) = A(2,1)
     B(2,2) = Z * B(2,2)
  END IF

  ! swap the rows if necessary
  IF (A(1,1) .LT. A(2,1)) THEN
     Z = U(1,1)
     U(1,1) = U(2,1)
     U(2,1) = Z

     Z = U(1,2)
     U(1,2) = U(2,2)
     U(2,2) = Z

     Z = B(1,1)
     B(1,1) = B(2,1)
     B(2,1) = Z

     Z = B(1,2)
     B(1,2) = B(2,2)
     B(2,2) = Z

     X = A(1,1)
     A(1,1) = A(2,1)
     A(2,1) = X

     X = A(1,2)
     A(1,2) = A(2,2)
     A(2,2) = X
  END IF

  ! compute the Givens rotation
  IF (A(1,1) .EQ. ZERO) THEN
     TANG = ZERO
  ELSE ! non-zero B
     TANG = A(2,1) / A(1,1)
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
     ! recompute the magnitudes in the second column
     A(1,2) = ABS(B(1,2))
     A(2,2) = ABS(B(2,2))
  END IF
  B(2,1) = CZERO

  ! make B(1,2) real and non-negative
  IF (AIMAG(B(1,2)) .EQ. ZERO) THEN
     IF (SIGN(ONE, REAL(B(1,2))) .NE. ONE) THEN
        B(1,2) = -B(1,2)
        B(2,2) = -B(2,2)
        V(1,2) = -V(1,2)
        V(2,2) = -V(2,2)
     END IF
  ELSE ! the general case
     Z = CONJG(B(1,2)) / A(1,2)
     B(1,2) = A(1,2)
     B(2,2) = B(2,2) * Z
     V(1,2) = V(1,2) * Z
     V(2,2) = V(2,2) * Z
  END IF

  ! make B(2,2) real and non-negative
  IF (AIMAG(B(2,2)) .EQ. ZERO) THEN
     IF (SIGN(ONE, REAL(B(2,2))) .NE. ONE) THEN
        U(2,1) = -U(2,1)
        U(2,2) = -U(2,2)
        B(2,2) = -B(2,2)
     END IF
  ELSE ! the general case
     Z = CONJG(B(2,2)) / A(2,2)
     U(2,1) = Z * U(2,1)
     U(2,2) = Z * U(2,2)
     B(2,2) = A(2,2)
  END IF

  ! B is now real so copy it to A
  A(1,1) = REAL(B(1,1))
  A(1,2) = REAL(B(1,2))
  A(2,2) = REAL(B(2,2))

  ! exit if A is a nul-matrix
  IF (A(1,1) .EQ. ZERO) GOTO 1

  ! divide by A(1,1)
  X = A(1,2) / A(1,1)
  Y = A(2,2) / A(1,1)

  ! the functions of \varphi
  IF (X .LE. Y) THEN
     T = SCALE(X, 1) * Y
  ELSE ! X > Y
     T = SCALE(Y, 1) * X
  END IF
  IF (T .EQ. ZERO) THEN
     TANF = ZERO
     SECF = ONE
  ELSE ! T > 0
     T = MIN(T / ((X - Y) * (X + Y) + ONE), ROOTH)
     TANF = T / (ONE + SQRT(T * T + ONE))
     SECF = SQRT(TANF * TANF + ONE)
  END IF

  ! the functions of \psi
  TANP = Y * TANF + X
  SECP = SQRT(TANP * TANP + ONE)

  ! the scaled singular values
  S(1) = (SECP / SECF) * A(1,1)
  S(2) = (SECF / SECP) * A(2,2)

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

  ! conjugate-transpose U
1 U(1,1) = CONJG(U(1,1))
  Z = CONJG(U(2,1))
  U(2,1) = CONJG(U(1,2))
  U(1,2) = Z
  U(2,2) = CONJG(U(2,2))
