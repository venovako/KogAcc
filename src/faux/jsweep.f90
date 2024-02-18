!>@brief \b JSWEEP fills in a look-up table O with S steps, each with P pairs, of a (quasi-)cyclic strategy J for a matrix of order N.
SUBROUTINE JSWEEP(J, N, S, P, O, INFO)
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: J, N
  INTEGER, INTENT(OUT) :: S, P, O(2,*), INFO

  INTEGER(KIND=c_int), ALLOCATABLE :: ARR(:,:)
  TYPE(c_ptr) :: JS
  INTEGER :: I, K, L
  INTEGER(KIND=c_int) :: ID, M

  TYPE(c_ptr), EXTERNAL :: PVN_CJS_INIT
  INTEGER(KIND=c_int), EXTERNAL :: PVN_CJS_NEXT, PVN_CJS_FREE

  INFO = 0
  S = 0
  P = 0
  IF (N .LT. 0) INFO = -2
  IF (J .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .LE. 1) RETURN

  SELECT CASE (J)
  CASE (0,1)
     P = 1
     IF (MOD(N, 2) .EQ. 0) THEN
        S = (N / 2) * (N - 1)
     ELSE ! N odd
        S = ((N - 1) / 2) * N
     END IF
  CASE (2)
     INFO = -MOD(N,2) * 2
     IF (INFO .EQ. 0) THEN
        P = N / 2
        S = N - 1
     END IF
  CASE (4)
     INFO = -MOD(N,2) * 2
     IF (INFO .EQ. 0) THEN
        P = N / 2
        S = N
     END IF
  CASE DEFAULT
     INFO = -1
  END SELECT
  IF (INFO .NE. 0) RETURN

  ID = INT(J, c_int)
  M = INT(N, c_int)
  JS = PVN_CJS_INIT(ID, M)
  IF (.NOT. C_ASSOCIATED(JS)) THEN
     INFO = 1
     RETURN
  END IF

  IF (J .EQ. 4) THEN
     ALLOCATE(ARR(4,P), STAT=L)
  ELSE ! J .NE. 4
     ALLOCATE(ARR(2,P), STAT=L)
  END IF
  IF (L .NE. 0) THEN
     INFO = 2
     RETURN
  END IF

  L = 1
  DO I = 1, S
     M = PVN_CJS_NEXT(JS, ARR)
     IF (M .LE. 0_c_int) THEN
        INFO = 3
        EXIT
     END IF
     SELECT CASE (J)
     CASE (0,1)
        O(1,L) = INT(ARR(1,1)) + 1
        O(2,L) = INT(ARR(2,1)) + 1
        L = L + 1
     CASE (2)
        DO K = 1, P
           O(1,L) = INT(ARR(1,K)) + 1
           O(2,L) = INT(ARR(2,K)) + 1
           L = L + 1
        END DO
     CASE (4)
        DO K = 1, P
           O(1,L) = INT(ARR(1,K)) + 1
           O(2,L) = INT(ARR(3,K)) + 1
           L = L + 1
        END DO
     CASE DEFAULT
        INFO = 4
        EXIT
     END SELECT
  END DO

  DEALLOCATE(ARR)
  M = PVN_CJS_FREE(JS)
  IF (M .NE. 0_c_int) INFO = 5
END SUBROUTINE JSWEEP
