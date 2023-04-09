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

  ! check if G has a non-finite value
  B(1,1) = ABS(G(1,1))
  IF (.NOT. (B(1,1) .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(2,1) = ABS(G(2,1))   
  IF (.NOT. (B(2,1) .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(1,2) = ABS(G(1,2))
  IF (.NOT. (B(1,2) .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF
  B(2,2) = ABS(G(2,2))
  IF (.NOT. (B(2,2) .LE. H)) THEN
     INFO = IERR
     RETURN
  END IF

  ! determine the G's structure
  I = 0
  L = IERR - 1
  J = L
  IF (B(1,1) .NE. ZERO) THEN
     I = IOR(I, 1)
     J = MAX(J, EXPONENT(B(1,1)))
  END IF
  IF (B(2,1) .NE. ZERO) THEN
     I = IOR(I, 2)
     J = MAX(J, EXPONENT(B(2,1)))
  END IF
  IF (B(1,2) .NE. ZERO) THEN
     I = IOR(I, 4)
     J = MAX(J, EXPONENT(B(1,2)))
  END IF
  IF (B(2,2) .NE. ZERO) THEN
     I = IOR(I, 8)
     J = MAX(J, EXPONENT(B(2,2)))
  END IF
  IF (J .EQ. L) J = 0

  IF (INFO .EQ. 0) THEN
     SELECT CASE (I)
     CASE (3, 5, 10, 12)
        INFO = EXPONENT(H) - J - 1
     CASE (7, 11, 13, 14, 15)
        INFO = EXPONENT(H) - J - 2
     END SELECT
  ELSE IF (INFO .LT. 0) THEN
     INFO = INFO + 1
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

  SELECT CASE (I)
  CASE (0)
     ! [ 0 0 ]
     ! [ 0 0 ]
     U(1,1) = SIGN(ONE, B(1,1))
     U(2,2) = SIGN(ONE, B(2,2))
     S(1) = ZERO
     S(2) = ZERO
  CASE (1)
     ! [ x 0 ]
     ! [ 0 0 ]
     U(1,1) = SIGN(ONE, B(1,1))
     U(2,2) = SIGN(ONE, B(2,2))
     S(1) = ABS(B(1,1))
     S(2) = ZERO
  CASE (2)
     ! [ 0 0 ]
     ! [ x 0 ]
     U(1,1) = ZERO
     U(2,1) = SIGN(ONE, B(1,2))
     U(1,2) = SIGN(ONE, B(2,1))
     U(2,2) = ZERO
     S(1) = ABS(B(2,1))
     S(2) = ZERO
  CASE (3)
     ! [ x 0 ]
     ! [ x 0 ]
     GOTO 1
  CASE (4)
     ! [ 0 x ]
     ! [ 0 0 ]
     U(1,1) = SIGN(ONE, B(1,2))
     U(2,2) = SIGN(ONE, B(2,1))
     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO
     S(1) = ABS(B(1,2))
     S(2) = ZERO
  CASE (5)
     ! [ x x ]
     ! [ 0 0 ]
     GOTO 2
  CASE (6)
     ! [ 0 x ]
     ! [ x 0 ]
     U(1,1) = SIGN(ONE, B(1,2))
     U(2,2) = SIGN(ONE, B(2,1))
     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO
     S(1) = ABS(B(1,2))
     S(2) = ABS(B(2,1))
  CASE (7)
     ! [ x x ]
     ! [ x 0 ]
     GOTO 3
  CASE (8)
     ! [ 0 0 ]
     ! [ 0 x ]
     U(1,1) = ZERO
     U(2,1) = SIGN(ONE, B(1,1))
     U(1,2) = SIGN(ONE, B(2,2))
     U(2,2) = ZERO
     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO
     S(1) = ABS(B(2,2))
     S(2) = ZERO
  CASE (9)
     ! [ x 0 ]
     ! [ 0 x ]
     U(1,1) = SIGN(ONE, B(1,1))
     U(2,2) = SIGN(ONE, B(2,2))
     S(1) = ABS(B(1,1))
     S(2) = ABS(B(2,2))
  CASE (10)
     ! [ 0 0 ]
     ! [ x x ]
     U(1,1) = ZERO
     U(2,1) = ONE
     U(1,2) = ONE
     U(2,2) = ZERO
     B(1,1) = B(2,1)
     B(1,2) = B(2,2)
     GOTO 2
  CASE (11)
     ! [ x 0 ]
     ! [ x x ]
     GOTO 3
  CASE (12)
     ! [ 0 x ]
     ! [ 0 x ]
     B(1,1) = B(1,2)
     B(2,1) = B(2,2)
     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO
     GOTO 1
  CASE (13)
     ! [ x x ]
     ! [ 0 x ]
     GOTO 3
  CASE (14)
     ! [ 0 x ]
     ! [ x x ]
     GOTO 3
  CASE (15)
     ! [ x x ]
     ! [ x x ]
     GOTO 3
  CASE DEFAULT
     INFO = L
     RETURN
  END SELECT
  GOTO 8

  ! [ x 0 ]
  ! [ x 0 ]
1 IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     U(1,1) = -ONE
     B(1,1) = -B(1,1)
  END IF
  IF (SIGN(ONE, B(2,1)) .NE. ONE) THEN
     U(2,2) = -ONE
     B(2,1) = -B(2,1)
  END IF
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
  END IF
  S(1) = CR_HYPOT(B(1,1), B(2,1))
  S(2) = ZERO
  ! compute the left Givens rotation
  TANG = B(2,1) / B(1,1)
#ifdef CR_MATH
  SECG = CR_HYPOT(TANG, ONE)
#else
#ifdef USE_IEEE_INTRINSIC
  SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#else
  SECG = SQRT(TANG * TANG + ONE)
#endif
#endif
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANL=', TANG, ', SECL=', SECG
#endif
  ! apply the left Givens rotation
  IF (TANG .GT. ZERO) THEN
#ifdef USE_IEEE_INTRINSIC
     X =  TANG
     Y = -TANG
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = IEEE_FMA(X, U(2,1), U(1,1)) / SECG
        U(2,1) = IEEE_FMA(Y,      Z, U(2,1)) / SECG
        Z = U(1,2)
        U(1,2) = IEEE_FMA(X, U(2,2), U(1,2)) / SECG
        U(2,2) = IEEE_FMA(Y,      Z, U(2,2)) / SECG
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = IEEE_FMA(X, U(2,1), U(1,1))
        U(2,1) = IEEE_FMA(Y,      Z, U(2,1))
        Z = U(1,2)
        U(1,2) = IEEE_FMA(X, U(2,2), U(1,2))
        U(2,2) = IEEE_FMA(Y,      Z, U(2,2))
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = (U(1,1) + TANG * U(2,1)) / SECG
        U(2,1) = (U(2,1) - TANG *      Z) / SECG
        Z = U(1,2)
        U(1,2) = (U(1,2) + TANG * U(2,2)) / SECG
        U(2,2) = (U(2,2) - TANG *      Z) / SECG
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = U(1,1) + TANG * U(2,1)
        U(2,1) = U(2,1) - TANG *      Z
        Z = U(1,2)
        U(1,2) = U(1,2) + TANG * U(2,2)
        U(2,2) = U(2,2) - TANG *      Z
     END IF
#endif
  END IF
  GOTO 8

  ! [ x x ]
  ! [ 0 0 ]
2 IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     B(1,1) = -B(1,1)
     V(1,1) = -ONE
  END IF
  IF (SIGN(ONE, B(1,2)) .NE. ONE) THEN
     B(1,2) = -B(1,2)
     V(2,2) = -ONE
  END IF
  IF (B(1,1) .LT. B(1,2)) THEN
     Y = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Y
     Z = V(1,1)
     V(1,1) = V(1,2)
     V(1,2) = Z
     Z = V(2,1)
     V(2,1) = V(2,2)
     V(2,2) = Z     
  END IF
  S(1) = CR_HYPOT(B(1,1), B(1,2))
  S(2) = ZERO
  ! compute the right Givens rotation
  TANG = B(1,2) / B(1,1)
#ifdef CR_MATH
  SECG = CR_HYPOT(TANG, ONE)
#else
#ifdef USE_IEEE_INTRINSIC
  SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#else
  SECG = SQRT(TANG * TANG + ONE)
#endif
#endif
#ifndef NDEBUG
  WRITE (ERROR_UNIT,9) 'TANR=', TANG, ', SECR=', SECG
#endif
  ! apply the right Givens rotation
  IF (TANG .GT. ZERO) THEN
#ifdef USE_IEEE_INTRINSIC
     X =  TANG
     Y = -TANG
     IF (SECG .GT. ONE) THEN
        Z = V(1,1)
        V(1,1) = IEEE_FMA(X, V(1,2), V(1,1)) / SECG
        V(1,2) = IEEE_FMA(Y,      Z, V(1,2)) / SECG
        Z = V(2,1)
        V(2,1) = IEEE_FMA(X, V(2,2), V(2,1)) / SECG
        V(2,2) = IEEE_FMA(Y,      Z, V(2,2)) / SECG
     ELSE ! SECG = 1
        Z = V(1,1)
        V(1,1) = IEEE_FMA(X, V(1,2), V(1,1))
        V(1,2) = IEEE_FMA(Y,      Z, V(1,2))
        Z = V(2,1)
        V(2,1) = IEEE_FMA(X, V(2,2), V(2,1))
        V(2,2) = IEEE_FMA(Y,      Z, V(2,2))
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = V(1,1)
        V(1,1) = (V(1,1) + TANG * V(1,2)) / SECG
        V(1,2) = (V(1,2) - TANG *      Z) / SECG
        Z = V(2,1)
        V(2,1) = (V(2,1) + TANG * V(2,2)) / SECG
        V(2,2) = (V(2,2) - TANG *      Z) / SECG
     ELSE ! SECG = 1
        Z = V(1,1)
        V(1,1) = V(1,1) + TANG * V(1,2)
        V(1,2) = V(1,2) - TANG *      Z
        Z = V(2,1)
        V(2,1) = V(2,1) + TANG * V(2,2)
        V(2,2) = V(2,2) - TANG *      Z
     END IF
#endif
  END IF
  GOTO 8

  ! TG2F/2 = (G11*G21 + G12*G22) / (G11*G11 - G21*G21 - G22*G22 + G12*G12)

3 A(1,1) = ABS(B(1,1))
  A(2,1) = ABS(B(2,1))
  A(1,2) = ABS(B(1,2))
  A(2,2) = ABS(B(2,2))
  IF (A(1,1) .GE. MAX(A(2,1), A(1,2), A(2,2))) THEN
     T = B(1,1)
     A = B / T
#ifdef USE_IEEE_INTRINSIC
     T = IEEE_FMA(A(1,2), A(2,2), A(2,1))
#else
     T = A(1,2) * A(2,2) + A(2,1)
#endif
  ELSE IF (A(2,1) .GE. MAX(A(1,1), A(1,2), A(2,2))) THEN
     T = B(2,1)
     A = B / T
#ifdef USE_IEEE_INTRINSIC
     T = IEEE_FMA(A(1,2), A(2,2), A(1,1))
#else
     T = A(1,2) * A(2,2) + A(1,1)
#endif
  ELSE IF (A(1,2) .GE. MAX(A(1,1), A(2,1), A(2,2))) THEN
     T = B(1,2)
     A = B / T
#ifdef USE_IEEE_INTRINSIC
     T = IEEE_FMA(A(1,1), A(2,1), A(2,2))
#else
     T = A(1,1) * A(2,1) + A(2,2)
#endif
  ELSE IF (A(2,2) .GE. MAX(A(1,1), A(2,1), A(1,2))) THEN
     T = B(2,2)
     A = B / T
#ifdef USE_IEEE_INTRINSIC
     T = IEEE_FMA(A(1,1), A(2,1), A(1,2))
#else
     T = A(1,1) * A(2,1) + A(1,2)
#endif
  ELSE ! should never happen
     INFO = L
     RETURN
  END IF

  ! \tan(2\varphi)
  IF (T .NE. ZERO) THEN
     X = CR_HYPOT(A(1,1), A(1,2))
     Y = CR_HYPOT(A(2,1), A(2,2))
     T = (TWO * T) / ((X - Y) * (X + Y))
  END IF
  ! \tan\varphi
  IF (T .EQ. ZERO) THEN
     TANF = SIGN(ZERO, T)
     SECF = ONE
  ELSE IF (ABS(T) .GT. H) THEN
     TANF = SIGN(ONE, T)
     SECF = ROOT2
  ELSE ! finite non-zero T
#ifdef CR_MATH
     TANF = CR_HYPOT(T, ONE)
#else
     T = SIGN(MIN(ABS(T), ROOTH), T)
#ifdef USE_IEEE_INTRINSIC
     TANF = SQRT(IEEE_FMA(T, T, ONE))
#else
     TANF = SQRT(T * T + ONE)
#endif
#endif
     TANF = T / (ONE + TANF)
#ifdef CR_MATH
     SECF = CR_HYPOT(TANF, ONE)
#else
#ifdef USE_IEEE_INTRINSIC
     SECF = SQRT(IEEE_FMA(TANF, TANF, ONE))
#else
     SECF = SQRT(TANF * TANF + ONE)
#endif
#endif
  END IF
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'TANF=', TANF, ', SECF=', SECF
#ifdef _OPENMP
  END IF
#endif
#endif
  COSF = ONE / SECF
  SINF = TANF * COSF

  ! TANP = (G12 + G22 * TANF) / (G11 + G21 * TANF) = (G11 * TANF - G21) / (G22 - G12 * TANF)

  ! \tan\psi
#ifdef USE_IEEE_INTRINSIC
  X = IEEE_FMA(B(2,2), TANF, B(1,2))
  Y = IEEE_FMA(B(2,1), TANF, B(1,1))
#else
  X = B(2,2) * TANF + B(1,2)
  Y = B(2,1) * TANF + B(1,1)
#endif
  IF (X .NE. ZERO) THEN
     TANP = X / Y
  ELSE ! X .EQ. ZERO
     TANP = ZERO
  END IF
#ifdef CR_MATH
  SECP = CR_HYPOT(TANP, ONE)
#else
#ifdef USE_IEEE_INTRINSIC
  SECP = SQRT(IEEE_FMA(TANP, TANP, ONE))
#else
  SECP = SQRT(TANP * TANP + ONE)
#endif
#endif
  COSP = ONE / SECP
  IF (ABS(TANP) .LE. H) THEN
     SINP = TANP * COSP
  ELSE ! TANP .EQ. \pm\infty
     SINP = SIGN(ONE, TANP)
  END IF

  ! U^T
  U(1,1) =  COSF
  U(2,1) = -SINF
  U(1,2) =  SINF
  U(2,2) =  COSF
  ! V
  V(1,1) =  COSP
  V(2,1) =  SINP
  V(1,2) = -SINP
  V(2,2) =  COSP

  ! \sigma
#ifdef USE_IEEE_INTRINSIC
  S(1) = IEEE_FMA(Y, COSP, X * SINP) * COSF
  X = IEEE_FMA(B(1,1), TANF, -B(2,1))
  Y = IEEE_FMA(-B(1,2), TANF, B(2,2))
  ! TANP = X / Y
  S(2) = IEEE_FMA(Y, COSP, X * SINP) * COSF
#else
  S(1) = (Y * COSP + X * SINP) * COSF
  X = B(1,1) * TANF - B(2,1)
  Y = B(2,2) - B(1,2) * TANF
  ! TANP = X / Y
  S(2) = (Y * COSP + X * SINP) * COSF
#endif

  IF (SIGN(ONE, S(1)) .NE. ONE) THEN
     U(1,1) = -U(1,1)
     U(1,2) = -U(1,2)
     S(1) = -S(1)
  END IF
  IF (SIGN(ONE, S(2)) .NE. ONE) THEN
     U(2,1) = -U(2,1)
     U(2,2) = -U(2,2)
     S(2) = -S(2)
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
  Z = U(2,1)
  U(2,1) = U(1,2)
  U(1,2) = Z
