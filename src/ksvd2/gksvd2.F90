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

  IF (INFO .EQ. 0) THEN
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
#ifdef USE_IEEE_INTRINSIC
        SECG = SQRT(IEEE_FMA(TANG, TANG, ONE))
#else
        SECG = SQRT(TANG * TANG + ONE)
#endif
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
        Z = B(1,2)
        B(1,2) = IEEE_FMA(X, B(2,2), B(1,2)) / SECG
        B(2,2) = IEEE_FMA(Y,      Z, B(2,2)) / SECG
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = IEEE_FMA(X, U(2,1), U(1,1))
        U(2,1) = IEEE_FMA(Y,      Z, U(2,1))
        Z = U(1,2)
        U(1,2) = IEEE_FMA(X, U(2,2), U(1,2))
        U(2,2) = IEEE_FMA(Y,      Z, U(2,2))
        Z = B(1,2)
        B(1,2) = IEEE_FMA(X, B(2,2), B(1,2))
        B(2,2) = IEEE_FMA(Y,      Z, B(2,2))
     END IF
#else
     IF (SECG .GT. ONE) THEN
        Z = U(1,1)
        U(1,1) = (U(1,1) + TANG * U(2,1)) / SECG
        U(2,1) = (U(2,1) - TANG *      Z) / SECG
        Z = U(1,2)
        U(1,2) = (U(1,2) + TANG * U(2,2)) / SECG
        U(2,2) = (U(2,2) - TANG *      Z) / SECG
        Z = B(1,2)
        B(1,2) = (B(1,2) + TANG * B(2,2)) / SECG
        B(2,2) = (B(2,2) - TANG *      Z) / SECG
     ELSE ! SECG = 1
        Z = U(1,1)
        U(1,1) = U(1,1) + TANG * U(2,1)
        U(2,1) = U(2,1) - TANG *      Z
        Z = U(1,2)
        U(1,2) = U(1,2) + TANG * U(2,2)
        U(2,2) = U(2,2) - TANG *      Z
        Z = B(1,2)
        B(1,2) = B(1,2) + TANG * B(2,2)
        B(2,2) = B(2,2) - TANG *      Z
     END IF
#endif
  END IF
#ifndef NDEBUG
  B(2,1) = ZERO
#endif

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
  IF (SIGN(ONE, B(2,2)) .NE. ONE) THEN
     IF (U(2,1) .NE. ZERO) U(2,1) = -U(2,1)
     IF (U(2,2) .NE. ZERO) U(2,2) = -U(2,2)
     B(2,2) = -B(2,2)
  END IF

  ! recompute the norm of the second column if needed
  IF (TANG .NE. ZERO) THEN
     IF (B(2,2) .EQ. ZERO) THEN
        S(2) = B(1,2)
     ELSE IF (B(1,2) .EQ. ZERO) THEN
        S(2) = B(2,2)
        ! B is diagonal
        GOTO 8
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
     INFO = IERR - 1
     RETURN
  END IF
#endif
  ! division by B(1,1)
  ! [ 1 X ]
  ! [ 0 Y ]
  X = B(1,2) / B(1,1)
  Y = B(2,2) / B(1,1)
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

#ifdef USE_IEEE_INTRINSIC
  ! update U, S
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
  ! update V, S
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
#else
  ! update U, S
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
  ! update V, S
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
#endif
#ifndef NDEBUG
#ifdef _OPENMP
  IF (OMP_GET_NUM_THREADS() .LE. 1) THEN
#endif
     WRITE (ERROR_UNIT,9) 'S(1)=', S(1), ', S(2)=', S(2)
#ifdef _OPENMP
  END IF
#endif
#endif

  ! symmetric permutation if S(1) < S(2) exceptionally
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
