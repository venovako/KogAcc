  EX = EXPONENT(X) - EXPONENT(T)
  FX = FRACTION(X) / FRACTION(T)
#ifndef NDEBUG
  EX = EX + EXPONENT(FX)
  FX = FRACTION(FX)
#endif
  TANP = SCALE(FX, EX) ! X/T
  EY = EXPONENT(Y) - EXPONENT(T)
  FY = FRACTION(Y) / FRACTION(T)
#ifndef NDEBUG
  EY = EY + EXPONENT(FY)
  FY = FRACTION(FY)
#endif
  SECP = SCALE(FY, EY) ! Y/T

  ! \tan(2\varphi)
  IF ((EX .EQ. EY) .AND. (FX .EQ. FY)) THEN
     ET = (1 + EX) + EY
     FT = FX * FY
#ifndef NDEBUG
     ET = ET + EXPONENT(FT)
     FT = FRACTION(FT)
#endif
     TANF = SCALE(FT, ET)
  ELSE IF (SECP .EQ. ONE) THEN
     ET = 1 - EX
     FT = ONE / FX
#ifndef NDEBUG
     ET = ET + EXPONENT(FT)
     FT = FRACTION(FT)
#endif
     TANF = SCALE(FT, ET)
  ELSE IF (TANP .EQ. ONE) THEN
#ifdef USE_IEEE_INTRINSIC
     TANF = IEEE_FMA(-SECP, SECP, TWO)
#else
     TANF = TWO - SECP * SECP
#endif
     ET = EXPONENT(TANF)
     FT = FRACTION(TANF)
     ET = (1 + EY) - ET
     FT = FY / FT
#ifndef NDEBUG
     ET = ET + EXPONENT(FT)
     FT = FRACTION(FT)
#endif
     TANF = SCALE(FT, ET)
  ELSE ! the general case
     IF ((EX .LT. EY) .OR. ((EX .EQ. EY) .AND. (FX .LE. FY))) THEN
        ET = EX - EY
#ifdef USE_IEEE_INTRINSIC
        TANF = IEEE_FMA(SCALE(IEEE_FMA(SCALE(ONE, ET), FX, -FY), EY), SCALE(IEEE_FMA(SCALE(ONE, ET), FX, FY), EY), ONE)
#else
        TANF = SCALE(SCALE(ONE, ET) * FX - FY, EY) * SCALE(SCALE(ONE, ET) * FX + FY, EY) + ONE
#endif
        ET = EXPONENT(TANF)
        FT = FRACTION(TANF)
        IF (ABS(TANF) .GT. ONE) THEN
           ET = (1 + EY) - ET
           FT = FY / FT
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           ET = ET + EX
           FT = FT * FX
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           TANF = SCALE(FT, ET)
        ELSE IF (TANF .NE. ZERO) THEN
           ET = (1 + EX) - ET
           FT = FX / FT
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           ET = ET + EY
           FT = FT * FY
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           TANF = SCALE(FT, ET)
        ELSE ! TANF .EQ. ZERO
           TANF = ONE / TANF
        END IF
     ELSE ! X > Y
        ET = EY - EX
#ifdef USE_IEEE_INTRINSIC
        TANF = IEEE_FMA(SCALE(IEEE_FMA(SCALE(-ONE, ET), FY, FX), EX), SCALE(IEEE_FMA(SCALE(ONE, ET), FY, FX), EX), ONE)
#else
        TANF = SCALE(SCALE(-ONE, ET) * FY + FX, EX) * SCALE(SCALE(ONE, ET) * FY + FX, EX) + ONE
#endif
        ET = EXPONENT(TANF)
        FT = FRACTION(TANF)
        IF (ABS(TANF) .GT. ONE) THEN
           ET = (1 + EX) - ET
           FT = FX / FT
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           ET = ET + EY
           FT = FT * FY
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           TANF = SCALE(FT, ET)
        ELSE IF (TANF .NE. ZERO) THEN
           ET = (1 + EY) - ET
           FT = FY / FT
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           ET = ET + EX
           FT = FT * FX
#ifndef NDEBUG
           ET = ET + EXPONENT(FT)
           FT = FRACTION(FT)
#endif
           TANF = SCALE(FT, ET)
        ELSE ! TANF .EQ. ZERO
           TANF = ONE / TANF
        END IF
     END IF
  END IF
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'TG2F=', TANF, ',ROOTH=', ROOTH
#ifdef _OPENMP
  END IF
#endif
#endif

  ! the functions of \varphi
  IF (TANF .EQ. ZERO) THEN
     TANF = SIGN(ZERO, TANF)
     SECF = ONE
  ELSE IF (.NOT. (ABS(TANF) .LE. H)) THEN
     TANF = SIGN(ONE, TANF)
     SECF = ROOT2
  ELSE ! finite non-zero
     ! SECF = sec(2\varphi)
#ifdef CR_MATH
     SECF = CR_HYPOT(TANF, ONE)
#else
     SECF = SIGN(MIN(ABS(TANF), ROOTH), TANF)
#ifdef USE_IEEE_INTRINSIC
     SECF = SQRT(IEEE_FMA(SECF, SECF, ONE))
#else
     SECF = SQRT(SECF * SECF + ONE)
#endif
#endif
     TANF = TANF / (ONE + SECF)
     ! SECF = sec(\varphi)
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
  TANP = IEEE_FMA(Y, TANF, X) / T
#else
  TANP = (Y * TANF + X) / T
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
