  ! \tan(2\varphi)
  IF ((X .EQ. ZERO) .OR. (Y .EQ. ZERO)) THEN
     T = ZERO
  ELSE IF (X .EQ. Y) THEN
     T = (TWO * X) * Y
  ELSE IF (Y .EQ. ONE) THEN
     T = TWO / X
  ELSE IF (X .EQ. ONE) THEN
#ifdef USE_IEEE_INTRINSIC
     T = IEEE_FMA(-Y, Y, TWO)
#else
     T = TWO - Y * Y
#endif
     T = (TWO * Y) / T
  ELSE ! the general case
#ifdef USE_IEEE_INTRINSIC
     T = IEEE_FMA(X - Y, X + Y, ONE)
#else
     T = (X - Y) * (X + Y) + ONE
#endif
     IF (ABS(T) .GT. ONE) THEN
        T = MIN(X, Y) * ((TWO * MAX(X, Y)) / T)
     ELSE ! |T| <= 1
        T = MAX(X, Y) * ((TWO * MIN(X, Y)) / T)
     END IF
  END IF
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'TG2F=', T, ',ROOTH=', ROOTH
#ifdef _OPENMP
  END IF
#endif
#endif

  ! the functions of \varphi
  IF (T .EQ. ZERO) THEN
     TANF = SIGN(ZERO, T)
     SECF = ONE
  ELSE IF (.NOT. (ABS(T) .LE. H)) THEN
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
     SECF = ABS(TANF)
     ! should always be true
     IF (SECF .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
        SECF = SQRT(IEEE_FMA(TANF, TANF, ONE))
#else
        SECF = SQRT(TANF * TANF + ONE)
#endif
     END IF
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

  ! the functions of \psi
#ifdef USE_IEEE_INTRINSIC
  TANP = IEEE_FMA(Y, TANF, X)
#else
  TANP = Y * TANF + X
#endif
#ifdef CR_MATH
  SECP = CR_HYPOT(TANP, ONE)
#else
  SECP = ABS(TANP)
  ! should always be true
  IF (SECP .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
     SECP = SQRT(IEEE_FMA(TANP, TANP, ONE))
#else
     SECP = SQRT(TANP * TANP + ONE)
#endif
  END IF
#endif
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'TANP=', TANP, ', SECP=', SECP
#ifdef _OPENMP
  END IF
#endif
#endif
