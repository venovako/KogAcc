  ! This is the generic part of the complex Kogbetliantz routines.

#ifdef USE_IEEE_INTRINSIC
#define CMUL(A,B) CMPLX(IEEE_FMA(REAL(A),REAL(B),-AIMAG(A)*AIMAG(B)),IEEE_FMA(REAL(A),AIMAG(B),AIMAG(A)*REAL(B)),K)
#else
#define CMUL(A,B) A * B
#endif

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
  IF (.NOT. (Y .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  X = ABS(REAL(G(2,2)))
  IF (.NOT. (X .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(2,2) = CMPLX(X, Y, K)
  Y = ABS(AIMAG(G(1,2)))
  IF (.NOT. (Y .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  X = ABS(REAL(G(1,2)))
  IF (.NOT. (X .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(1,2) = CMPLX(X, Y, K)
  Y = ABS(AIMAG(G(2,1)))
  IF (.NOT. (Y .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  X = ABS(REAL(G(2,1)))
  IF (.NOT. (X .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(2,1) = CMPLX(X, Y, K)
  Y = ABS(AIMAG(G(1,1)))
  IF (.NOT. (Y .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  X = ABS(REAL(G(1,1)))
  IF (.NOT. (X .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(1,1) = CMPLX(X, Y, K)

  ! determine the G's structure
  I = 0
  L = IERR - 1
  J = L
  X = REAL(B(1,1))
  Y = AIMAG(B(1,1))
  IF (X .NE. ZERO) THEN
     I = IOR(I, 1)
     J = MAX(J, EXPONENT(X))
  END IF
  IF (Y .NE. ZERO) THEN
     I = IOR(I, 1)
     J = MAX(J, EXPONENT(Y))
  END IF
  X = REAL(B(2,1))
  Y = AIMAG(B(2,1))
  IF (X .NE. ZERO) THEN
     I = IOR(I, 2)
     J = MAX(J, EXPONENT(X))
  END IF
  IF (Y .NE. ZERO) THEN
     I = IOR(I, 2)
     J = MAX(J, EXPONENT(Y))
  END IF
  X = REAL(B(1,2))
  Y = AIMAG(B(1,2))
  IF (X .NE. ZERO) THEN
     I = IOR(I, 4)
     J = MAX(J, EXPONENT(X))
  END IF
  IF (Y .NE. ZERO) THEN
     I = IOR(I, 4)
     J = MAX(J, EXPONENT(Y))
  END IF
  X = REAL(B(2,2))
  Y = AIMAG(B(2,2))
  IF (X .NE. ZERO) THEN
     I = IOR(I, 8)
     J = MAX(J, EXPONENT(X))
  END IF
  IF (Y .NE. ZERO) THEN
     I = IOR(I, 8)
     J = MAX(J, EXPONENT(Y))
  END IF
  IF (J .EQ. L) J = 0

  IF (INFO .EQ. 0) THEN
     SELECT CASE (I)
     CASE (0)
        CONTINUE
     CASE (1, 2, 4, 8)
        INFO = EXPONENT(H) - J - 1
     CASE DEFAULT
        INFO = EXPONENT(H) - J - 2
     END SELECT
  ELSE IF (INFO .LT. 0) THEN
     INFO = INFO + 1
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

  SELECT CASE (I)
  CASE (0)
     ! [ 0 0 ]
     ! [ 0 0 ]
     U(1,1) = CMPLX(SIGN(ONE, REAL(B(1,1))), ZERO, K)
     U(2,2) = CMPLX(SIGN(ONE, REAL(B(2,2))), ZERO, K)
     S(1) = ZERO
     S(2) = ZERO
  CASE (1)
     ! [ x 0 ]
     ! [ 0 0 ]
     IF (A(1,1) .GT. ZERO) THEN
        U(1,1) = CONJG(B(1,1)) / A(1,1)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     U(2,2) = CMPLX(SIGN(ONE, REAL(B(2,2))), ZERO, K)
     S(1) = A(1,1)
     S(2) = ZERO
  CASE (2)
     ! [ 0 0 ]
     ! [ x 0 ]
     U(1,1) = CZERO
     U(2,1) = CMPLX(SIGN(ONE, REAL(B(1,2))), ZERO, K)
     IF (A(2,1) .GT. ZERO) THEN
        U(1,2) = CONJG(B(2,1)) / A(2,1)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     U(2,2) = CZERO
     S(1) = A(2,1)
     S(2) = ZERO
  CASE (3)
     ! [ x 0 ]
     ! [ x 0 ]
     GOTO 1
  CASE (4)
     ! [ 0 x ]
     ! [ 0 0 ]
     IF (A(1,2) .GT. ZERO) THEN
        U(1,1) = CONJG(B(1,2)) / A(1,2)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     U(2,2) = CMPLX(SIGN(ONE, REAL(B(2,1))), ZERO, K)
     V(1,1) = CZERO
     V(2,1) = CONE
     V(1,2) = CONE
     V(2,2) = CZERO
     S(1) = A(1,2)
     S(2) = ZERO
  CASE (5)
     ! [ x x ]
     ! [ 0 0 ]
     GOTO 2
  CASE (6)
     ! [ 0 x ]
     ! [ x 0 ]
     IF (A(1,2) .GT. ZERO) THEN
        U(1,1) = CONJG(B(1,2)) / A(1,2)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     IF (A(2,1) .GT. ZERO) THEN
        U(2,2) = CONJG(B(2,1)) / A(2,1)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     V(1,1) = CZERO
     V(2,1) = CONE
     V(1,2) = CONE
     V(2,2) = CZERO
     S(1) = A(1,2)
     S(2) = A(2,1)
  CASE (7)
     ! [ x x ]
     ! [ x 0 ]
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z
     B(2,2) = B(2,1)
     Y = A(1,1)
     A(1,1) = A(1,2)
     A(1,2) = Y
     A(2,2) = A(2,1)
     V(1,1) = CZERO
     V(2,1) = CONE
     V(1,2) = CONE
     V(2,2) = CZERO
     GOTO 3
  CASE (8)
     ! [ 0 0 ]
     ! [ 0 x ]
     U(1,1) = CZERO
     U(2,1) = CMPLX(SIGN(ONE, REAL(B(1,1))), ZERO, K)
     IF (A(2,2) .GT. ZERO) THEN
        U(1,2) = CONJG(B(2,2)) / A(2,2)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     U(2,2) = CZERO
     V(1,1) = CZERO
     V(2,1) = CONE
     V(1,2) = CONE
     V(2,2) = CZERO
     S(1) = A(2,2)
     S(2) = ZERO
  CASE (9)
     ! [ x 0 ]
     ! [ 0 x ]
     IF (A(1,1) .GT. ZERO) THEN
        U(1,1) = CONJG(B(1,1)) / A(1,1)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     IF (A(2,2) .GT. ZERO) THEN
        U(2,2) = CONJG(B(2,2)) / A(2,2)
     ELSE ! should never happen
        INFO = L
        RETURN
     END IF
     S(1) = A(1,1)
     S(2) = A(2,2)
  CASE (10)
     ! [ 0 0 ]
     ! [ x x ]
     U(1,1) = CZERO
     U(2,1) = CONE
     U(1,2) = CONE
     U(2,2) = CZERO
     B(1,1) = B(2,1)
     B(1,2) = B(2,2)
     A(1,1) = A(2,1)
     A(1,2) = A(2,2)
     GOTO 2
  CASE (11)
     ! [ x 0 ]
     ! [ x x ]
     U(1,1) = CZERO
     U(2,1) = CONE
     U(1,2) = CONE
     U(2,2) = CZERO
     Z = B(1,1)
     B(1,1) = B(2,2)
     B(2,2) = Z
     B(1,2) = B(2,1)
     Y = A(1,1)
     A(1,1) = A(2,2)
     A(2,2) = Y
     A(1,2) = A(2,1)
     V(1,1) = CZERO
     V(2,1) = CONE
     V(1,2) = CONE
     V(2,2) = CZERO     
     GOTO 3
  CASE (12)
     ! [ 0 x ]
     ! [ 0 x ]
     B(1,1) = B(1,2)
     B(2,1) = B(2,2)
     A(1,1) = A(1,2)
     A(2,1) = A(2,2)
     V(1,1) = CZERO
     V(2,1) = CONE
     V(1,2) = CONE
     V(2,2) = CZERO
     GOTO 1
  CASE (13)
     ! [ x x ]
     ! [ 0 x ]
     GOTO 3
  CASE (14)
     ! [ 0 x ]
     ! [ x x ]
     U(1,1) = CZERO
     U(2,1) = CONE
     U(1,2) = CONE
     U(2,2) = CZERO
     B(1,1) = B(2,1)
     Z = B(1,2)
     B(1,2) = B(2,2)
     B(2,2) = Z
     A(1,1) = A(2,1)
     X = A(1,2)
     A(1,2) = A(2,2)
     A(2,2) = X
     GOTO 3
  CASE (15)
     ! [ x x ]
     ! [ x x ]
     GOTO 4
  CASE DEFAULT
     INFO = L
     RETURN
  END SELECT
  GOTO 8

  ! [ x 0 ]
  ! [ x 0 ]
1 IF (A(1,1) .GT. ZERO) THEN
     Z = CONJG(B(1,1)) / A(1,1)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  U(1,1) = CMUL(Z, U(1,1))
  U(1,2) = CMUL(Z, U(1,2))
  B(1,1) = CMPLX(A(1,1), ZERO, K)
  IF (A(2,1) .GT. ZERO) THEN
     Z = CONJG(B(2,1)) / A(2,1)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  U(2,1) = CMUL(Z, U(2,1))
  U(2,2) = CMUL(Z, U(2,2))
  B(2,1) = CMPLX(A(2,1), ZERO, K)
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
     X = A(1,1)
     A(1,1) = A(2,1)
     A(2,1) = X
  END IF
  S(1) = CR_HYPOT(A(1,1), A(2,1))
  S(2) = ZERO
  ! compute the left Givens rotation
  TANG = A(2,1) / A(1,1)
#ifdef CR_MATH
  SECG = CR_HYPOT(TANG, ONE)
#else
  SECG = ABS(TANG)
  ! should always be true
  IF (SECG .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
     SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#else
     SECG = SQRT(TANG * TANG + ONE)
#endif
  END IF
#endif
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANL=', TANG, ', SECL=', SECG
#endif
  ! apply the left Givens rotation
  IF (TANG .GT. ZERO) THEN
     X =  TANG
     Y = -TANG
#ifdef USE_IEEE_INTRINSIC
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))) / SECG, IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))) / SECG, K)
        U(2,1) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,1))) / SECG, IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,1))) / SECG, K)
        Z = U(1,2)
        U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))) / SECG, IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))) / SECG, K)
        U(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,2))) / SECG, IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,2))) / SECG, K)
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))), IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))), K)
        U(2,1) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,1))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,1))), K)
        Z = U(1,2)
        U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))), IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))), K)
        U(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(U(2,2))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(U(2,2))), K)
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = (U(1,1) + X * U(2,1)) / SECG
        U(2,1) = (U(2,1) + Y *      Z) / SECG
        Z = U(1,2)
        U(1,2) = (U(1,2) + X * U(2,2)) / SECG
        U(2,2) = (U(2,2) + Y *      Z) / SECG
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = U(1,1) + X * U(2,1)
        U(2,1) = U(2,1) + Y *      Z
        Z = U(1,2)
        U(1,2) = U(1,2) + X * U(2,2)
        U(2,2) = U(2,2) + Y *      Z
     END IF
#endif
  END IF
  GOTO 8

  ! [ x x ]
  ! [ 0 0 ]
2 IF (A(1,1) .GT. ZERO) THEN
     Z = CONJG(B(1,1)) / A(1,1)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  V(1,1) = CMUL(V(1,1), Z)
  V(2,1) = CMUL(V(2,1), Z)
  B(1,1) = CMPLX(A(1,1), ZERO, K)
  IF (A(1,2) .GT. ZERO) THEN
     Z = CONJG(B(1,2)) / A(1,2)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  V(1,2) = CMUL(V(1,2), Z)
  V(2,2) = CMUL(V(2,2), Z)
  B(1,2) = CMPLX(A(1,2), ZERO, K)
  IF (A(1,1) .LT. A(1,2)) THEN
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z
     Y = A(1,1)
     A(1,1) = A(1,2)
     A(1,2) = Y
     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z
     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z
  END IF
  S(1) = CR_HYPOT(A(1,1), A(1,2))
  S(2) = ZERO
  ! compute the right Givens rotation
  TANG = A(1,2) / A(1,1)
#ifdef CR_MATH
  SECG = CR_HYPOT(TANG, ONE)
#else
  SECG = ABS(TANG)
  ! should always be true
  IF (SECG .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
     SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#else
     SECG = SQRT(TANG * TANG + ONE)
#endif
  END IF
#endif
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANR=', TANG, ', SECR=', SECG
#endif
  ! apply the right Givens rotation
  IF (TANG .GT. ZERO) THEN
     X =  TANG
     Y = -TANG
#ifdef USE_IEEE_INTRINSIC
     IF (SECG .GT. ONE) THEN
        Z = V(1,1)
        V(1,1) = CMPLX(IEEE_FMA(X, REAL(V(1,2)), REAL(V(1,1))) / SECG, IEEE_FMA(X, AIMAG(V(1,2)), AIMAG(V(1,1))) / SECG, K)
        V(1,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(1,2))) / SECG, IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(1,2))) / SECG, K)
        Z = V(2,1)
        V(2,1) = CMPLX(IEEE_FMA(X, REAL(V(2,2)), REAL(V(2,1))) / SECG, IEEE_FMA(X, AIMAG(V(2,2)), AIMAG(V(2,1))) / SECG, K)
        V(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(2,2))) / SECG, IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(2,2))) / SECG, K)
     ELSE ! SECG = 1
        Z = V(1,1)
        V(1,1) = CMPLX(IEEE_FMA(X, REAL(V(1,2)), REAL(V(1,1))), IEEE_FMA(X, AIMAG(V(1,2)), AIMAG(V(1,1))), K)
        V(1,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(1,2))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(1,2))), K)
        Z = V(2,1)
        V(2,1) = CMPLX(IEEE_FMA(X, REAL(V(2,2)), REAL(V(2,1))), IEEE_FMA(X, AIMAG(V(2,2)), AIMAG(V(2,1))), K)
        V(2,2) = CMPLX(IEEE_FMA(Y,      REAL(Z), REAL(V(2,2))), IEEE_FMA(Y,      AIMAG(Z), AIMAG(V(2,2))), K)
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = V(1,1)
        V(1,1) = (V(1,1) + X * V(1,2)) / SECG
        V(1,2) = (V(1,2) + Y *      Z) / SECG
        Z = V(2,1)
        V(2,1) = (V(2,1) + X * V(2,2)) / SECG
        V(2,2) = (V(2,2) + Y *      Z) / SECG
     ELSE ! SECG = 1
        Z = V(1,1)
        V(1,1) = V(1,1) + X * V(1,2)
        V(1,2) = V(1,2) + Y *      Z
        Z = V(2,1)
        V(2,1) = V(2,1) + X * V(2,2)
        V(2,2) = V(2,2) + Y *      Z
     END IF
#endif
  END IF
  GOTO 8

  ! [ x x ]
  ! [ 0 x ]
3 IF (A(1,1) .GT. ZERO) THEN
     Z = CONJG(B(1,1)) / A(1,1)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  U(1,1) = CMUL(Z, U(1,1))
  U(1,2) = CMUL(Z, U(1,2))
  B(1,1) = CMPLX(A(1,1), ZERO, K)
  B(1,2) = CMUL(Z, B(1,2))
  A(1,2) = CR_HYPOT(REAL(B(1,2)), AIMAG(B(1,2)))
  IF (A(1,2) .GT. ZERO) THEN
     Z = CONJG(B(1,2)) / A(1,2)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  B(1,2) = CMPLX(A(1,2), ZERO, K)
  B(2,2) = CMUL(B(2,2), Z)
  A(2,2) = CR_HYPOT(REAL(B(2,2)), AIMAG(B(2,2)))
  V(1,2) = CMUL(V(1,2), Z)
  V(2,2) = CMUL(V(2,2), Z)
  IF (A(2,2) .GT. ZERO) THEN
     Z = CONJG(B(2,2)) / A(2,2)
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF
  U(2,1) = CMUL(Z, U(2,1))
  U(2,2) = CMUL(Z, U(2,2))
  B(2,2) = CMPLX(A(2,2), ZERO, K)
  IF (A(1,1) .GE. MAX(A(1,2), A(2,2))) THEN
     S(1) = A(1,1)
     S(2) = CR_HYPOT(A(1,2), A(2,2))
     GOTO 5
  END IF
  B(2,1) = CZERO
  A(2,1) = ZERO

  ! [ x x ]
  ! [ ? x ]
  ! compute the first column norm
4 IF (A(2,1) .EQ. ZERO) THEN
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

  ! swap the columns if necessary
  IF (S(1) .LT. S(2)) THEN
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z
     Z = B(2,1)
     B(2,1) = B(2,2)
     B(2,2) = Z
     Y = A(1,1)
     A(1,1) = A(1,2)
     A(1,2) = Y
     Y = A(2,1)
     A(2,1) = A(2,2)
     A(2,2) = Y
     Y = S(1)
     S(1) = S(2)
     S(2) = Y
     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z
     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z
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
  IF (A(1,1) .GT. ZERO) THEN
     Z = CONJG(B(1,1)) / A(1,1)
     U(1,1) = CMUL(Z, U(1,1))
     U(1,2) = CMUL(Z, U(1,2))
     B(1,1) = CMPLX(A(1,1), ZERO, K)
     B(1,2) = CMUL(Z, B(1,2))
     A(1,2) = CR_HYPOT(REAL(B(1,2)), AIMAG(B(1,2)))
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF

  ! make B(2,1) real and non-negative
  IF (A(2,1) .GT. ZERO) THEN
     Z = CONJG(B(2,1)) / A(2,1)
     U(2,1) = CMUL(Z, U(2,1))
     U(2,2) = CMUL(Z, U(2,2))
     B(2,1) = CMPLX(A(2,1), ZERO, K)
     B(2,2) = CMUL(Z, B(2,2))
     A(2,2) = CR_HYPOT(REAL(B(2,2)), AIMAG(B(2,2)))
  ELSE ! ignore the signs of the real and the imaginary zero
     B(2,1) = CZERO
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
        SECG = ABS(TANG)
        ! should always be true
        IF (SECG .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
           SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#else
           SECG = SQRT(TANG * TANG + ONE)
#endif
        END IF
#endif
     ELSE ! TANG = 0
        SECG = ONE
     END IF
  END IF
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'TANG=', TANG, ', SECG=', SECG
#ifdef _OPENMP
  END IF
#endif
#endif

  ! apply the Givens rotation
  A(1,1) = S(1)
  B(1,1) = CMPLX(A(1,1), ZERO, K)
  IF (TANG .GT. ZERO) THEN
     X =  TANG
     Y = -TANG
#ifdef USE_IEEE_INTRINSIC
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))) / SECG, IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))) / SECG, K)
        U(2,1) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,1))) / SECG, IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,1))) / SECG, K)
        Z = U(1,2)
        U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))) / SECG, IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))) / SECG, K)
        U(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,2))) / SECG, IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,2))) / SECG, K)
        Z = B(1,2)
        B(1,2) = CMPLX(IEEE_FMA(X, REAL(B(2,2)), REAL(B(1,2))) / SECG, IEEE_FMA(X, AIMAG(B(2,2)), AIMAG(B(1,2))) / SECG, K)
        B(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(B(2,2))) / SECG, IEEE_FMA(Y, AIMAG(     Z), AIMAG(B(2,2))) / SECG, K)
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))), IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))), K)
        U(2,1) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,1))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,1))), K)
        Z = U(1,2)
        U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))), IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))), K)
        U(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,2))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,2))), K)
        Z = B(1,2)
        B(1,2) = CMPLX(IEEE_FMA(X, REAL(B(2,2)), REAL(B(1,2))), IEEE_FMA(X, AIMAG(B(2,2)), AIMAG(B(1,2))), K)
        B(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(B(2,2))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(B(2,2))), K)
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = (U(1,1) + X * U(2,1)) / SECG
        U(2,1) = (U(2,1) + Y *      Z) / SECG
        Z = U(1,2)
        U(1,2) = (U(1,2) + X * U(2,2)) / SECG
        U(2,2) = (U(2,2) + Y *      Z) / SECG
        Z = B(1,2)
        B(1,2) = (B(1,2) + X * B(2,2)) / SECG
        B(2,2) = (B(2,2) + Y *      Z) / SECG
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = U(1,1) + X * U(2,1)
        U(2,1) = U(2,1) + Y *      Z
        Z = U(1,2)
        U(1,2) = U(1,2) + X * U(2,2)
        U(2,2) = U(2,2) + Y *      Z
        Z = B(1,2)
        B(1,2) = B(1,2) + X * B(2,2)
        B(2,2) = B(2,2) + Y *      Z
     END IF
#endif
     ! recompute the magnitudes in the second column
     A(1,2) = CR_HYPOT(REAL(B(1,2)), AIMAG(B(1,2)))
     A(2,2) = CR_HYPOT(REAL(B(2,2)), AIMAG(B(2,2)))
  END IF

  ! make B(1,2) real and non-negative
  IF (A(1,2) .GT. ZERO) THEN
     Z = CONJG(B(1,2)) / A(1,2)
     B(1,2) = CMPLX(A(1,2), ZERO, K)
     B(2,2) = CMUL(B(2,2), Z)
     A(2,2) = CR_HYPOT(REAL(B(2,2)), AIMAG(B(2,2)))
     V(1,2) = CMUL(V(1,2), Z)
     V(2,2) = CMUL(V(2,2), Z)
  ELSE ! ignore the signs of the real and the imaginary zero
     B(1,2) = CZERO
  END IF

  ! make B(2,2) real and non-negative
  IF (A(2,2) .GT. ZERO) THEN
     Z = CONJG(B(2,2)) / A(2,2)
     U(2,1) = CMUL(Z, U(2,1))
     U(2,2) = CMUL(Z, U(2,2))
     B(2,2) = CMPLX(A(2,2), ZERO, K)
  ELSE IF (SIGN(ONE, REAL(B(2,2))) .NE. ONE) THEN
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
     B(2,2) = CZERO
  ELSE ! ignore the sign of the imaginary zero
     B(2,2) = CZERO
  END IF

  ! recompute the norm of the second column
  IF (A(2,2) .EQ. ZERO) THEN
     S(2) = A(1,2)
  ELSE IF (A(1,2) .EQ. ZERO) THEN
     S(2) = A(2,2)
  ELSE ! full second column
     S(2) = CR_HYPOT(A(1,2), A(2,2))
  END IF
  IF (A(1,2) .EQ. ZERO) GOTO 8

#ifndef NDEBUG
  ! internal consistency check
  IF ((.NOT. (A(1,1) .LE. H)) .OR. (A(1,1) .LE. ZERO)) THEN
#ifdef _OPENMP
     IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
        WRITE (ERROR_UNIT,9) 'A_11=', A(1,1), ',    H=', H
#ifdef _OPENMP
     END IF
#endif
     INFO = L
     RETURN
  END IF
#endif
  ! division by A(1,1)
  ! [ 1 X ]
  ! [ 0 Y ]
5 T = A(1,1)
  X = A(1,2) / T
  Y = A(2,2) / T
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'X_12=', X, ', Y_22=', Y
#ifdef _OPENMP
  END IF
#endif
#endif
  IF (X .EQ. ZERO) GOTO 8

  ! execute the upper-triangular SVD procedure
#include "gksvdu.F90"

  ! update U
  X =  TANF
  Y = -TANF
#ifdef USE_IEEE_INTRINSIC
  IF (SECF .NE. ONE) THEN
     Z = U(1,1)
     U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))) / SECF, IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))) / SECF, K)
     U(2,1) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,1))) / SECF, IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,1))) / SECF, K)
     Z = U(1,2)
     U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))) / SECF, IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))) / SECF, K)
     U(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,2))) / SECF, IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,2))) / SECF, K)
  ELSE ! SECF = 1
     Z = U(1,1)
     U(1,1) = CMPLX(IEEE_FMA(X, REAL(U(2,1)), REAL(U(1,1))), IEEE_FMA(X, AIMAG(U(2,1)), AIMAG(U(1,1))), K)
     U(2,1) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,1))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,1))), K)
     Z = U(1,2)
     U(1,2) = CMPLX(IEEE_FMA(X, REAL(U(2,2)), REAL(U(1,2))), IEEE_FMA(X, AIMAG(U(2,2)), AIMAG(U(1,2))), K)
     U(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(U(2,2))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(U(2,2))), K)
  END IF
#else
    IF (SECF .NE. ONE) THEN
     Z = U(1,1)
     U(1,1) = (U(1,1) + X * U(2,1)) / SECF
     U(2,1) = (U(2,1) + Y *      Z) / SECF
     Z = U(1,2)
     U(1,2) = (U(1,2) + X * U(2,2)) / SECF
     U(2,2) = (U(2,2) + Y *      Z) / SECF
  ELSE ! SECF = 1
     Z = U(1,1)
     U(1,1) = U(1,1) + X * U(2,1)
     U(2,1) = U(2,1) + Y *      Z
     Z = U(1,2)
     U(1,2) = U(1,2) + X * U(2,2)
     U(2,2) = U(2,2) + Y *      Z
  END IF
#endif

  ! update S
  IF (SECF .NE. ONE) THEN
     S(1) = (SECP / SECF) * A(1,1) ! the first scaled singular value
  ELSE ! SECF = 1
     S(1) = SECP * A(1,1) ! the first scaled singular value
  END IF
  IF (SECP .NE. ONE) THEN
     S(2) = (SECF / SECP) * A(2,2) ! the second scaled singular value
  ELSE ! SECP = 1
     S(2) = SECF * A(2,2) ! the second scaled singular value
  END IF
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'S(1)=', S(1), ', S(2)=', S(2)
#ifdef _OPENMP
  END IF
#endif
#endif

  ! update V
  X =  TANP
  Y = -TANP
#ifdef USE_IEEE_INTRINSIC
  IF (SECP .NE. ONE) THEN
     Z = V(1,1)
     V(1,1) = CMPLX(IEEE_FMA(X, REAL(V(1,2)), REAL(V(1,1))) / SECP, IEEE_FMA(X, AIMAG(V(1,2)), AIMAG(V(1,1))) / SECP, K)
     V(1,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(V(1,2))) / SECP, IEEE_FMA(Y, AIMAG(     Z), AIMAG(V(1,2))) / SECP, K)
     Z = V(2,1)
     V(2,1) = CMPLX(IEEE_FMA(X, REAL(V(2,2)), REAL(V(2,1))) / SECP, IEEE_FMA(X, AIMAG(V(2,2)), AIMAG(V(2,1))) / SECP, K)
     V(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(V(2,2))) / SECP, IEEE_FMA(Y, AIMAG(     Z), AIMAG(V(2,2))) / SECP, K)
  ELSE ! SECP = 1
     Z = V(1,1)
     V(1,1) = CMPLX(IEEE_FMA(X, REAL(V(1,2)), REAL(V(1,1))), IEEE_FMA(X, AIMAG(V(1,2)), AIMAG(V(1,1))), K)
     V(1,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(V(1,2))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(V(1,2))), K)
     Z = V(2,1)
     V(2,1) = CMPLX(IEEE_FMA(X, REAL(V(2,2)), REAL(V(2,1))), IEEE_FMA(X, AIMAG(V(2,2)), AIMAG(V(2,1))), K)
     V(2,2) = CMPLX(IEEE_FMA(Y, REAL(     Z), REAL(V(2,2))), IEEE_FMA(Y, AIMAG(     Z), AIMAG(V(2,2))), K)
  END IF
#else
  IF (SECP .NE. ONE) THEN
     Z = V(1,1)
     V(1,1) = (V(1,1) + X * V(1,2)) / SECP
     V(1,2) = (V(1,2) + Y *      Z) / SECP
     Z = V(2,1)
     V(2,1) = (V(2,1) + X * V(2,2)) / SECP
     V(2,2) = (V(2,2) + Y *      Z) / SECP
  ELSE ! SECP = 1
     Z = V(1,1)
     V(1,1) = V(1,1) + X * V(1,2)
     V(1,2) = V(1,2) + Y *      Z
     Z = V(2,1)
     V(2,1) = V(2,1) + X * V(2,2)
     V(2,2) = V(2,2) + Y *      Z
  END IF
#endif

  ! symmetric permutation if S(1) < S(2)
8 IF (S(1) .LT. S(2)) THEN
     Z = U(1,1)
     U(1,1) = U(2,1)
     U(2,1) = Z
     Z = U(1,2)
     U(1,2) = U(2,2)
     U(2,2) = Z
     Y = S(1)
     S(1) = S(2)
     S(2) = Y
     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z
     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z
  END IF

  ! conjugate-transpose U
  U(1,1) = CONJG(U(1,1))
  Z = CONJG(U(2,1))
  U(2,1) = CONJG(U(1,2))
  U(1,2) = Z
  U(2,2) = CONJG(U(2,2))
