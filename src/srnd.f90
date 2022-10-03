!>@brief SRND generates a set of pseudorandom single precision real 2x2 matrices.
PROGRAM SRND
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
  IMPLICIT NONE
  REAL, PARAMETER :: ZERO = 0.0
  INTEGER, ALLOCATABLE :: ISEED(:)
  REAL, ALLOCATABLE :: H(:)
  CHARACTER(LEN=64) :: CLA
  INTEGER :: SSIZE, N, I
  LOGICAL :: UPPER
  REAL :: T

  CALL RANDOM_SEED(SIZE=SSIZE)
  IF (SSIZE .LE. 0) ERROR STOP 'seed size non-positive'
  I = COMMAND_ARGUMENT_COUNT()
  IF (I .LT. 2) THEN
     IF (SSIZE .GT. 1) THEN
        WRITE (CLA,'(I1)') SSIZE
        CLA = 'srnd.exe ((U|u)[pper]|(G|g)[eneral]) N [SEED1 ... SEED'//TRIM(CLA)//']'
     ELSE ! SSIZE = 1
        CLA = 'srnd.exe ((U|u)[pper]|(G|g)[eneral]) N [SEED1]'
     END IF
     WRITE (ERROR_UNIT,*) CLA
     ERROR STOP 'All SEED arguments have to be given, or none of them.'
  END IF
  CALL GET_COMMAND_ARGUMENT(2, CLA)
  READ (CLA,*) N
  IF (N .LT. 0) ERROR STOP 'the second argument is invalid'
  CALL GET_COMMAND_ARGUMENT(1, CLA)
  SELECT CASE (CLA(1:1))
  CASE ('G')
     UPPER = .FALSE.
     T = TINY(ZERO)
  CASE ('U')
     UPPER = .TRUE.
     T = TINY(ZERO)
  CASE ('g')
     UPPER = .FALSE.
     T = ZERO
  CASE ('u')
     UPPER = .TRUE.
     T = ZERO
  CASE DEFAULT
     ERROR STOP 'the first argument is invalid'
  END SELECT
  IF (I .EQ. 2) THEN
     ALLOCATE(ISEED(SSIZE))
     CALL RANDOM_SEED(GET=ISEED)
     DO I = 1, SSIZE
        WRITE (ERROR_UNIT,*) ISEED(I)
     END DO
  ELSE IF (I .EQ. (SSIZE + 2)) THEN
     ALLOCATE(ISEED(SSIZE))
     DO I = 1, SSIZE
        CALL GET_COMMAND_ARGUMENT(I + 2, CLA)
        READ (CLA,*) ISEED(I)
     END DO
     CALL RANDOM_SEED(PUT=ISEED)
  ELSE ! a wrong SEED
     ERROR STOP 'invalid number of SEED arguments'
  END IF
  IF (UPPER) THEN
     ALLOCATE(H(6))
     DO I = 1, N
        CALL RANDOM_NUMBER(H)
        IF ((H(1) .GT. ZERO) .AND. (H(1) .LT. T)) H(1) = H(1) + T
        IF (MOD(EXPONENT(H(2)), 2) .NE. 0) H(1) = -H(1)
        WRITE (*,1,ADVANCE='NO') '', H(1)
        WRITE (*,1,ADVANCE='NO') ' ', ZERO
        IF ((H(3) .GT. ZERO) .AND. (H(3) .LT. T)) H(3) = H(3) + T
        IF (MOD(EXPONENT(H(4)), 2) .NE. 0) H(3) = -H(3)
        WRITE (*,1,ADVANCE='NO') ' ', H(3)
        IF ((H(5) .GT. ZERO) .AND. (H(5) .LT. T)) H(5) = H(5) + T
        IF (MOD(EXPONENT(H(6)), 2) .NE. 0) H(5) = -H(5)
        WRITE (*,1) ' ', H(5)
     END DO
  ELSE ! general
     ALLOCATE(H(8))
     DO I = 1, N
        CALL RANDOM_NUMBER(H)
        IF ((H(1) .GT. ZERO) .AND. (H(1) .LT. T)) H(1) = H(1) + T
        IF (MOD(EXPONENT(H(2)), 2) .NE. 0) H(1) = -H(1)
        WRITE (*,1,ADVANCE='NO') '', H(1)
        IF ((H(3) .GT. ZERO) .AND. (H(3) .LT. T)) H(3) = H(3) + T
        IF (MOD(EXPONENT(H(4)), 2) .NE. 0) H(3) = -H(3)
        WRITE (*,1,ADVANCE='NO') ' ', H(3)
        IF ((H(5) .GT. ZERO) .AND. (H(5) .LT. T)) H(5) = H(5) + T
        IF (MOD(EXPONENT(H(6)), 2) .NE. 0) H(5) = -H(5)
        WRITE (*,1,ADVANCE='NO') ' ', H(5)
        IF ((H(7) .GT. ZERO) .AND. (H(7) .LT. T)) H(7) = H(7) + T
        IF (MOD(EXPONENT(H(8)), 2) .NE. 0) H(7) = -H(7)
        WRITE (*,1) ' ', H(7)
     END DO
  END IF
  DEALLOCATE(H)
  DEALLOCATE(ISEED)
1 FORMAT(A,ES16.9E2)
END PROGRAM SRND
