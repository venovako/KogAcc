  ! a partial fix of the Y >= 1, X > 0 problem
  IF (Y .EQ. ONE) THEN
     T = TWO / X
  ELSE IF (X .EQ. ONE) THEN
     T = TWO * Y
#ifdef USE_IEEE_INTRINSIC
     T = T / IEEE_FMA(-Y, Y, TWO)
#else
     T = T / (TWO - Y * Y)
#endif
  ELSE IF (X .EQ. Y) THEN
     T = (TWO * X) * X
  ELSE IF (X .LT. Y) THEN
     T = (TWO * X) * Y
#ifdef USE_IEEE_INTRINSIC
     T = T / IEEE_FMA(X, X, IEEE_FMA(-Y, Y, ONE))
#else
     T = T / ((ONE - Y * Y) + X * X)
#endif
  ELSE ! X > Y
     T = (TWO * Y) * X
     ! a possible underflow of X-Y is safe so it does not have to be avoided
#ifdef USE_IEEE_INTRINSIC
     T = T / IEEE_FMA(X - Y, X + Y, ONE)
#else
     T = T / ((X - Y) * (X + Y) + ONE)
#endif
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
  ! Negative tan(2\varphi) can happen only if Y is the largest element by magnitude
  ! (impossible mathematically), what in turn, with the correctly rounded hypot,
  ! can happen only as a consequence of the pivoted QR factorization.
  IF (T .EQ. ZERO) THEN
     TANF = ZERO
     SECF = ONE
  ELSE IF (ABS(T) .GT. H) THEN
     TANF = SIGN(ONE, T)
     SECF = SQRT(TWO)
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

  ! the functions of \psi
#ifdef USE_IEEE_INTRINSIC
  TANP = IEEE_FMA(Y, TANF, X)
#else
  TANP = Y * TANF + X
#endif
#ifdef CR_MATH
  SECP = CR_HYPOT(TANP, ONE)
#else
#ifdef USE_IEEE_INTRINSIC
  SECP = SQRT(IEEE_FMA(TANP, TANP, ONE))
#else
  SECP = SQRT(TANP * TANP + ONE)
#endif
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
