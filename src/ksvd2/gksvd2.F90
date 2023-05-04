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
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z
     B(2,2) = B(2,1)
     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO
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
     U(1,1) = ZERO
     U(2,1) = ONE
     U(1,2) = ONE
     U(2,2) = ZERO
     Z = B(1,1)
     B(1,1) = B(2,2)
     B(2,2) = Z
     B(1,2) = B(2,1)
     V(1,1) = ZERO
     V(2,1) = ONE
     V(1,2) = ONE
     V(2,2) = ZERO     
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
     U(1,1) = ZERO
     U(2,1) = ONE
     U(1,2) = ONE
     U(2,2) = ZERO
     B(1,1) = B(2,1)
     Z = B(1,2)
     B(1,2) = B(2,2)
     B(2,2) = Z
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
     Z = B(1,1)
     B(1,1) = B(2,1)
     B(2,1) = Z
  END IF
  S(1) = CR_HYPOT(B(1,1), B(2,1))
  S(2) = ZERO
  ! compute the left Givens rotation
  TANG = B(2,1) / B(1,1)
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
2 IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     B(1,1) = -B(1,1)
     V(1,1) = -ONE
  END IF
  IF (SIGN(ONE, B(1,2)) .NE. ONE) THEN
     B(1,2) = -B(1,2)
     V(2,2) = -ONE
  END IF
  IF (B(1,1) .LT. B(1,2)) THEN
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z
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
3 IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     IF (U(1,1) .NE. ZERO) THEN
        U(1,1) = -U(1,1)
     ELSE ! U(1,2) has to be non-zero
        U(1,2) = -U(1,2)
     END IF
     B(1,1) = -B(1,1)
     B(1,2) = -B(1,2)
  END IF
  IF (SIGN(ONE, B(1,2)) .NE. ONE) THEN
     B(1,2) = -B(1,2)
     B(2,2) = -B(2,2)
     IF (V(1,2) .NE. ZERO) THEN
        V(1,2) = -V(1,2)
     ELSE ! V(2,2) has to be non-zero
        V(2,2) = -V(2,2)
     END IF
  END IF
  IF (SIGN(ONE, B(2,2)) .NE. ONE) THEN
     IF (U(2,1) .NE. ZERO) THEN
        U(2,1) = -U(2,1)
     ELSE ! U(2,2) has to be non-zero
        U(2,2) = -U(2,2)
     END IF
     B(2,2) = -B(2,2)
  END IF
  IF (B(1,1) .GE. MAX(B(1,2), B(2,2))) THEN
     TANG = ZERO
     SECG = ONE
     GOTO 5
  END IF
  B(2,1) = ZERO

  ! [ x x ]
  ! [ ? x ]
  ! compute the first column norm
4 IF (B(2,1) .EQ. ZERO) THEN
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

  ! swap the columns and their norms if necessary
  IF (S(1) .LT. S(2)) THEN
     Z = B(1,1)
     B(1,1) = B(1,2)
     B(1,2) = Z
     Z = B(2,1)
     B(2,1) = B(2,2)
     B(2,2) = Z
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
  IF (ABS(B(1,1)) .LT. ABS(B(2,1))) THEN
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
  END IF

  ! make B(1,1) non-negative
  IF (SIGN(ONE, B(1,1)) .NE. ONE) THEN
     IF (U(1,1) .NE. ZERO) THEN
        U(1,1) = -U(1,1)
     ELSE ! U(1,2) .NE. ZERO
        U(1,2) = -U(1,2)
     END IF
     B(1,1) = -B(1,1)
     B(1,2) = -B(1,2)
  END IF

  ! make B(2,1) non-negative
  IF (SIGN(ONE, B(2,1)) .NE. ONE) THEN
     IF (U(2,1) .NE. ZERO) THEN
        U(2,1) = -U(2,1)
     ELSE ! U(2,2) .NE. ZERO
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
  B(1,1) = S(1)
  IF (TANG .GT. ZERO) THEN
     X =  TANG
     Y = -TANG
#ifdef USE_IEEE_INTRINSIC
     IF (SECG .GT. ONE) THEN
        Z = B(1,2)
        B(1,2) = IEEE_FMA(X, B(2,2), B(1,2)) / SECG
        B(2,2) = IEEE_FMA(Y,      Z, B(2,2)) / SECG
     ELSE ! SECG = 1
        Z = B(1,2)
        B(1,2) = IEEE_FMA(X, B(2,2), B(1,2))
        B(2,2) = IEEE_FMA(Y,      Z, B(2,2))
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = B(1,2)
        B(1,2) = (B(1,2) + X * B(2,2)) / SECG
        B(2,2) = (B(2,2) + Y *      Z) / SECG
     ELSE ! SECG = 1
        Z = B(1,2)
        B(1,2) = B(1,2) + X * B(2,2)
        B(2,2) = B(2,2) + Y *      Z
     END IF
#endif
  END IF

  ! make B(1,2) non-negative
  IF (SIGN(ONE, B(1,2)) .NE. ONE) THEN
     B(1,2) = -B(1,2)
     B(2,2) = -B(2,2)
     IF (V(1,2) .NE. ZERO) THEN
        V(1,2) = -V(1,2)
     ELSE ! V(2,2) .NE. ZERO
        V(2,2) = -V(2,2)
     END IF
  END IF

  ! make B(2,2) non-negative
  SECG = SIGN(ONE, B(2,2))
  IF (SECG .NE. ONE) B(2,2) = -B(2,2)

  ! recompute the norm of the second column if needed
  IF (TANG .NE. ZERO) THEN
     IF (B(2,2) .EQ. ZERO) THEN
        S(2) = B(1,2)
     ELSE IF (B(1,2) .EQ. ZERO) THEN
        S(2) = B(2,2)
     ELSE ! full second column
        S(2) = CR_HYPOT(B(1,2), B(2,2))
     END IF
  ELSE IF (B(1,2) .EQ. ZERO) THEN
     ! B is diagonal
     GOTO 8
  END IF

#ifndef NDEBUG
  ! internal consistency check
  IF ((.NOT. (B(1,1) .LE. H)) .OR. (B(1,1) .LE. ZERO)) THEN
#ifdef _OPENMP
     IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
        WRITE (ERROR_UNIT,9) 'B_11=', B(1,1), ',    H=', H
#ifdef _OPENMP
     END IF
#endif
     INFO = L
     RETURN
  END IF
#endif

  ! division by B(1,1)
  ! [ 1 X ]
  ! [ 0 Y ]
5 T = B(1,1)
  X = B(1,2) / T
  Y = B(2,2) / T
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'X_12=', X, ', Y_22=', Y
#ifdef _OPENMP
  END IF
#endif
#endif

  ! execute the upper-triangular SVD procedure
#include "gksvdu.F90"

  ! T = TAN
  ! X = COS
6 IF (SECG .EQ. ONE) THEN
     T = TANF + TANG
#ifdef USE_IEEE_INTRINSIC
     T = T / IEEE_FMA(-TANF, TANG, ONE)
#else
     T = T / (ONE - TANF * TANG)
#endif
     SECG = ABS(T)
     IF (SECG .EQ. ZERO) THEN
        X = ONE
        Y = SIGN(ZERO, T)
     ELSE IF (.NOT. (SECG .LE. HUGE(SECG))) THEN
        X = ZERO
        Y = SIGN(ONE, T)
     ELSE ! the general case
#ifdef CR_MATH
        SECG = CR_HYPOT(T, ONE)
#else
        IF (SECG .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
           SECG = SQRT(IEEE_FMA(T, T, ONE))
#else
           SECG = SQRT(T * T + ONE)
#endif
        END IF
#endif
        ! Y = SIN
        IF (.NOT. (SECG .LE. HUGE(SECG))) THEN
           X = ZERO
           Y = SIGN(ONE, T)
        ELSE ! the general case
           X = ONE / SECG
           Y = T / SECG
        END IF
     END IF
     ! update U
     Z = U(1,1)
     U(1,1) = X * U(1,1) + Y * U(2,1)
     U(2,1) = X * U(2,1) - Y *      Z
     Z = U(1,2)
     U(1,2) = X * U(1,2) + Y * U(2,2)
     U(2,2) = X * U(2,2) - Y *      Z
  ELSE ! SECG = -1
     T = TANF - TANG
#ifdef USE_IEEE_INTRINSIC
     T = T / IEEE_FMA(TANF, TANG, ONE)
#else
     T = T / (ONE + TANF * TANG)
#endif
     SECG = ABS(T)
     IF (SECG .EQ. ZERO) THEN
        X = ONE
        Y = SIGN(ZERO, T)
     ELSE IF (.NOT. (SECG .LE. HUGE(SECG))) THEN
        X = ZERO
        Y = SIGN(ONE, T)
     ELSE ! the general case
#ifdef CR_MATH
        SECG = CR_HYPOT(T, ONE)
#else
        IF (SECG .LT. ROOTH) THEN
#ifdef USE_IEEE_INTRINSIC
           SECG = SQRT(IEEE_FMA(T, T, ONE))
#else
           SECG = SQRT(T * T + ONE)
#endif
        END IF
#endif
        ! Y = -SIN
        IF (.NOT. (SECG .LE. HUGE(SECG))) THEN
           X = ZERO
           Y = -SIGN(ONE, T)
        ELSE ! the general case
           X = ONE / SECG
           Y = -T / SECG
        END IF
     END IF
     ! update U
     Z = U(1,1)
     U(1,1) = Y * U(2,1) + X * U(1,1)
     U(2,1) = Y *      Z - X * U(2,1)
     Z = U(1,2)
     U(1,2) = Y * U(2,2) + X * U(1,2)
     U(2,2) = Y *      Z - X * U(2,2)
  END IF

  ! update S
  IF (SECF .NE. ONE) THEN
     S(1) = (SECP / SECF) * B(1,1) ! the first scaled singular value
  ELSE ! SECF = 1
     S(1) = SECP * B(1,1) ! the first scaled singular value
  END IF
  IF (SECP .NE. ONE) THEN
     S(2) = (SECF / SECP) * B(2,2) ! the second scaled singular value
  ELSE ! SECP = 1
     S(2) = SECF * B(2,2) ! the second scaled singular value
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
     V(1,1) = IEEE_FMA(X, V(1,2), V(1,1)) / SECP
     V(1,2) = IEEE_FMA(Y,      Z, V(1,2)) / SECP
     Z = V(2,1)
     V(2,1) = IEEE_FMA(X, V(2,2), V(2,1)) / SECP
     V(2,2) = IEEE_FMA(Y,      Z, V(2,2)) / SECP
  ELSE ! SECP = 1
     Z = V(1,1)
     V(1,1) = IEEE_FMA(X, V(1,2), V(1,1))
     V(1,2) = IEEE_FMA(Y,      Z, V(1,2))
     Z = V(2,1)
     V(2,1) = IEEE_FMA(X, V(2,2), V(2,1))
     V(2,2) = IEEE_FMA(Y,      Z, V(2,2))
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

  ! transpose U
  Z = U(2,1)
  U(2,1) = U(1,2)
  U(1,2) = Z
