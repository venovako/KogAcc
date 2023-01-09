  ! This is the generic part of the complex Kogbetliantz routines.

#define CMUL(A,B) CMPLX(IEEE_FMA(REAL(A),REAL(B),-AIMAG(A)*AIMAG(B)),IEEE_FMA(REAL(A),AIMAG(B),AIMAG(A)*REAL(B)),K)

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
  A(1,1) = CR_HYPOT(REAL(B(1,1)), AIMAG(B(1,1)))
  A(2,1) = CR_HYPOT(REAL(B(2,1)), AIMAG(B(2,1)))
  A(1,2) = CR_HYPOT(REAL(B(1,2)), AIMAG(B(1,2)))
  A(2,2) = CR_HYPOT(REAL(B(2,2)), AIMAG(B(2,2)))

  ! compute the first column norm
  IF (A(2,1) .EQ. ZERO) THEN
     S(1) = A(1,1)
  ELSE IF (A(1,1) .EQ. ZERO) THEN
     S(1) = A(2,1)
  ELSE ! full 1st column
     S(1) = CR_HYPOT(A(1,1), A(2,1))
  END IF

  ! compute the second column norm
  IF (A(1,2) .EQ. ZERO) THEN
     S(2) = A(2,2)
  ELSE IF (A(2,2) .EQ. ZERO) THEN
     S(2) = A(1,2)
  ELSE ! full 2nd column
     S(2) = CR_HYPOT(A(1,2), A(2,2))
  END IF

  ! swap the columns if necessary, avoiding the QR (especially with a small angle) if possible
  IF ((S(1) .LT. S(2)) .OR. ((S(1) .EQ. S(2)) .AND. (A(1,1) .NE. ZERO) .AND. (A(2,1) .NE. ZERO) .AND. &
       ((A(1,2) .EQ. ZERO) .OR. (A(2,2) .EQ. ZERO) .OR. ((A(1,1) + A(2,1)) .LT. (A(1,2) + A(2,2)))))) THEN
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

  ! make B(1,1) real and non-negative
  IF (AIMAG(B(1,1)) .EQ. ZERO) THEN
     IF (SIGN(ONE, REAL(B(1,1))) .NE. ONE) THEN
        IF (U(1,1) .EQ. CZERO) THEN
           U(1,1) = CZERO
        ELSE ! change the sign
           U(1,1) = -U(1,1)
        END IF
        IF (U(1,2) .EQ. CZERO) THEN
           U(1,2) = CZERO
        ELSE ! change the sign
           U(1,2) = -U(1,2)
        END IF
        B(1,1) = CMPLX(-REAL(B(1,1)), ZERO, K)
        IF (B(1,2) .EQ. CZERO) THEN
           B(1,2) = CZERO
        ELSE ! change the sign
           B(1,2) = -B(1,2)
        END IF
     END IF
  ELSE ! the general case
     Z = CONJG(B(1,1)) / A(1,1)
     U(1,1) = Z
     B(1,1) = CMPLX(A(1,1), ZERO, K)
     B(1,2) = CMUL(Z, B(1,2))
  END IF

  ! make B(2,1) real and non-negative
  IF (AIMAG(B(2,1)) .EQ. ZERO) THEN
     IF (REAL(B(2,1)) .EQ. ZERO) THEN
        B(2,1) = CZERO
     ELSE IF (REAL(B(2,1)) .LT. ZERO) THEN
        IF (U(2,1) .EQ. CZERO) THEN
           U(2,1) = CZERO
        ELSE ! change the sign
           U(2,1) = -U(2,1)
        END IF
        IF (U(2,2) .EQ. CZERO) THEN
           U(2,2) = CZERO
        ELSE ! change the sign
           U(2,2) = -U(2,2)
        END IF
        B(2,1) = CMPLX(-REAL(B(2,1)), ZERO, K)
        B(2,2) = -B(2,2)
     END IF
  ELSE ! the general case
     Z = CONJG(B(2,1)) / A(2,1)
     U(2,2) = Z
     B(2,1) = CMPLX(A(2,1), ZERO, K)
     B(2,2) = CMUL(Z, B(2,2))
  END IF

  ! compute the Givens rotation
  IF (A(2,1) .EQ. ZERO) THEN
     TANG = ZERO
     SECG = ONE
  ELSE ! B not upper triangular
     TANG = A(2,1) / A(1,1)
     IF (TANG .GT. ZERO) THEN
#ifdef CR_MATH
        SECG = CR_HYPOT(TANG, ONE)
#else
        SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#endif
     ELSE ! TANG = 0
        SECG = ONE
     END IF
  END IF
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANG=', TANG, ', SECG=', SECG
#endif

  ! apply the Givens rotation
  B(1,1) = S(1)
  IF (TANG .GT. ZERO) THEN
     X =  TANG
     Y = -TANG
     IF (SECG .GT. ONE) THEN
        B(2,1) = U(1,1)
        U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))) / SECG, IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))) / SECG, K)
        U(2,1) = CMPLX(IEEE_FMA(Y, REAL(B(2,1)), REAL(U(2,1))) / SECG, IEEE_FMA(Y, AIMAG(B(2,1)), AIMAG(U(2,1))) / SECG, K)
        B(2,1) = U(1,2)
        U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))) / SECG, IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))) / SECG, K)
        U(2,2) = CMPLX(IEEE_FMA(Y, REAL(B(2,1)), REAL(U(2,2))) / SECG, IEEE_FMA(Y, AIMAG(B(2,1)), AIMAG(U(2,2))) / SECG, K)
        B(2,1) = B(1,2)
        B(1,2) = CMPLX(IEEE_FMA(X, REAL(B(2,2)), REAL(B(1,2))) / SECG, IEEE_FMA(X, AIMAG(B(2,2)), AIMAG(B(1,2))) / SECG, K)
        B(2,2) = CMPLX(IEEE_FMA(Y, REAL(B(2,1)), REAL(B(2,2))) / SECG, IEEE_FMA(Y, AIMAG(B(2,1)), AIMAG(B(2,2))) / SECG, K)
     ELSE ! SECG = 1
        B(2,1) = U(1,1)
        U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))), IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))), K)
        U(2,1) = CMPLX(IEEE_FMA(Y, REAL(B(2,1)), REAL(U(2,1))), IEEE_FMA(Y, AIMAG(B(2,1)), AIMAG(U(2,1))), K)
        B(2,1) = U(1,2)
        U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))), IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))), K)
        U(2,2) = CMPLX(IEEE_FMA(Y, REAL(B(2,1)), REAL(U(2,2))), IEEE_FMA(Y, AIMAG(B(2,1)), AIMAG(U(2,2))), K)
        B(2,1) = B(1,2)
        B(1,2) = CMPLX(IEEE_FMA(X, REAL(B(2,2)), REAL(B(1,2))), IEEE_FMA(X, AIMAG(B(2,2)), AIMAG(B(1,2))), K)
        B(2,2) = CMPLX(IEEE_FMA(Y, REAL(B(2,1)), REAL(B(2,2))), IEEE_FMA(Y, AIMAG(B(2,1)), AIMAG(B(2,2))), K)
     END IF
     ! recompute the magnitudes in the second column
     A(1,2) = CR_HYPOT(REAL(B(1,2)), AIMAG(B(1,2)))
     A(2,2) = CR_HYPOT(REAL(B(2,2)), AIMAG(B(2,2)))
  END IF
  B(2,1) = CZERO

  ! make B(1,2) real and non-negative
  IF (AIMAG(B(1,2)) .EQ. ZERO) THEN
     IF (REAL(B(1,2)) .EQ. ZERO) THEN
        B(1,2) = CZERO
     ELSE IF (REAL(B(1,2)) .LT. ZERO) THEN
        B(1,2) = CMPLX(-REAL(B(1,2)), ZERO, K)
        B(2,2) = -B(2,2)
        IF (V(1,2) .EQ. CZERO) THEN
           V(1,2) = CZERO
        ELSE ! change the sign
           V(1,2) = -V(1,2)
        END IF
        IF (V(2,2) .EQ. CZERO) THEN
           V(2,2) = CZERO
        ELSE ! change the sign
           V(2,2) = -V(2,2)
        END IF
     END IF
  ELSE ! the general case
     Z = CONJG(B(1,2)) / A(1,2)
     B(1,2) = CMPLX(A(1,2), ZERO, K)
     B(2,2) = CMUL(B(2,2), Z)
     V(1,2) = CMUL(V(1,2), Z)
     V(2,2) = CMUL(V(2,2), Z)
  END IF

  ! make B(2,2) real and non-negative
  IF (AIMAG(B(2,2)) .EQ. ZERO) THEN
     IF (SIGN(ONE, REAL(B(2,2))) .NE. ONE) THEN
        IF (U(2,1) .EQ. CZERO) THEN
           U(2,1) = CZERO
        ELSE ! change the sign
           U(2,1) = -U(2,1)
        END IF
        IF (U(2,2) .EQ. CZERO) THEN
           U(2,2) = CZERO
        ELSE ! change the sign
           U(2,2) = -U(2,2)
        END IF
        B(2,2) = CMPLX(-REAL(B(2,2)), ZERO, K)
     END IF
  ELSE ! the general case
     Z = CONJG(B(2,2)) / A(2,2)
     U(2,1) = CMUL(Z, U(2,1))
     U(2,2) = CMUL(Z, U(2,2))
     B(2,2) = CMPLX(A(2,2), ZERO, K)
  END IF

  ! B is now real so copy it to A
  A(1,1) = REAL(B(1,1))
  A(2,1) = REAL(B(2,1))
  A(1,2) = REAL(B(1,2))
  A(2,2) = REAL(B(2,2))

  ! exit if A is diagonal
  IF (A(1,2) .EQ. ZERO) GOTO 8

  ! divide by A(1,1)
  ! [ 1 x ]
  ! [ 0 y ]
  X = A(1,2) / A(1,1)
  Y = A(2,2) / A(1,1)
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) '   X=', X, ',    Y=', Y
#endif
  IF (X .EQ. ZERO) GOTO 8

  ! a partial fix
  IF (Y .EQ. ONE) THEN
     T = TWO / X
  ELSE IF (X .EQ. ONE) THEN
     T = SCALE(Y, 1)
     T = T / IEEE_FMA(-Y, Y, TWO)
  ELSE IF (X .EQ. Y) THEN
     T = SCALE(X, 1) * Y
  ELSE IF (X .LT. Y) THEN
     T = SCALE(X, 1) * Y
     T = T / IEEE_FMA(X, X, IEEE_FMA(-Y, Y, ONE))
  ELSE ! X > Y
     T = SCALE(Y, 1) * X
     T = T / IEEE_FMA(X - Y, X + Y, ONE)
  END IF
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) '   T=', T, ',ROOTH=', ROOTH
#endif

  ! the functions of \varphi
  ! Negative T can happen only if Y > 1 (impossible mathematically),
  ! what in turn, with the correctly rounded hypot, can happen only
  ! as a consequence of the QR factorization.
  IF (T .EQ. ZERO) THEN
     TANF = ZERO
     SECF = ONE
  ELSE IF (ABS(T) .GT. HUGE(T)) THEN
     TANF = SIGN(ONE, T)
     SECF = SQRT(TWO)
  ELSE ! finite non-zero T
#ifdef CR_MATH
     TANF = CR_HYPOT(T, ONE)
#else
     T = SIGN(MIN(ABS(T), ROOTH), T)
     TANF = SQRT(IEEE_FMA(T, T, ONE))
#endif
     TANF = T / (ONE + TANF)
#ifdef CR_MATH
     SECF = CR_HYPOT(TANF, ONE)
#else
     SECF = SQRT(IEEE_FMA(TANF, TANF, ONE))
#endif
  END IF
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANF=', TANF, ', SECF=', SECF
#endif

  ! the functions of \psi
  TANP = IEEE_FMA(Y, TANF, X)
#ifdef CR_MATH
  SECP = CR_HYPOT(TANP, ONE)
#else
  SECP = SQRT(IEEE_FMA(TANP, TANP, ONE))
#endif
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANP=', TANP, ', SECP=', SECP
#endif

  ! update U
  X =  TANF
  Y = -TANF
  IF (SECF .NE. ONE) THEN
     S(1) = (SECP / SECF) * A(1,1) ! the first scaled singular value
     Z = U(1,1)
     U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))) / SECF, IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))) / SECF, K)
     U(2,1) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,1))) / SECF, IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,1))) / SECF, K)
     Z = U(1,2)
     U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))) / SECF, IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))) / SECF, K)
     U(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,2))) / SECF, IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,2))) / SECF, K)
  ELSE ! SECF = 1
     S(1) = SECP * A(1,1) ! the first scaled singular value
     Z = U(1,1)
     U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))), IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))), K)
     U(2,1) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,1))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,1))), K)
     Z = U(1,2)
     U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))), IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))), K)
     U(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,2))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,2))), K)
  END IF

  ! update V
  X =  TANP
  Y = -TANP
  IF (SECP .NE. ONE) THEN
     S(2) = (SECF / SECP) * A(2,2) ! the second scaled singular value
     Z = V(1,1)
     V(1,1) = CMPLX(IEEE_FMA(X, REAL(V(1,2)), REAL(V(1,1))) / SECP, IEEE_FMA(X, AIMAG(V(1,2)), AIMAG(V(1,1))) / SECP, K)
     V(1,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(1,2))) / SECP, IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(1,2))) / SECP, K)
     Z = V(2,1)
     V(2,1) = CMPLX(IEEE_FMA(X, REAL(V(2,2)), REAL(V(2,1))) / SECP, IEEE_FMA(X, AIMAG(V(2,2)), AIMAG(V(2,1))) / SECP, K)
     V(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(2,2))) / SECP, IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(2,2))) / SECP, K)
  ELSE ! SECP = 1
     S(2) = SECF * A(2,2) ! the second scaled singular value
     Z = V(1,1)
     V(1,1) = CMPLX(IEEE_FMA(X, REAL(V(1,2)), REAL(V(1,1))), IEEE_FMA(X, AIMAG(V(1,2)), AIMAG(V(1,1))), K)
     V(1,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(1,2))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(1,2))), K)
     Z = V(2,1)
     V(2,1) = CMPLX(IEEE_FMA(X, REAL(V(2,2)), REAL(V(2,1))), IEEE_FMA(X, AIMAG(V(2,2)), AIMAG(V(2,1))), K)
     V(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(2,2))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(2,2))), K)
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

  ! conjugate-transpose U
  X = REAL(U(1,1))
  IF (X .EQ. ZERO) X = ZERO
  Y = AIMAG(U(1,1))
  IF (Y .EQ. ZERO) THEN
     U(1,1) = CMPLX(X, ZERO, K)
  ELSE ! complex
     U(1,1) = CMPLX(X, -Y, K)
  END IF
  X = REAL(U(2,1))
  IF (X .EQ. ZERO) X = ZERO
  Y = AIMAG(U(2,1))
  IF (Y .EQ. ZERO) THEN
     Z = CMPLX(X, ZERO, K)
  ELSE ! complex
     Z = CMPLX(X, -Y, K)
  END IF
  X = REAL(U(1,2))
  IF (X .EQ. ZERO) X = ZERO
  Y = AIMAG(U(1,2))
  IF (Y .EQ. ZERO) THEN
     U(2,1) = CMPLX(X, ZERO, K)
  ELSE ! complex
     U(2,1) = CMPLX(X, -Y, K)
  END IF
  U(1,2) = Z
  X = REAL(U(2,2))
  IF (X .EQ. ZERO) X = ZERO
  Y = AIMAG(U(2,2))
  IF (Y .EQ. ZERO) THEN
     U(2,2) = CMPLX(X, ZERO, K)
  ELSE ! complex
     U(2,2) = CMPLX(X, -Y, K)
  END IF
