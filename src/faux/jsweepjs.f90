!>@brief \b JSWEEP fills in a look-up table O with S steps, each with P pairs, of a (quasi-)cyclic strategy J for a matrix of order N.
SUBROUTINE JSWEEP(J, N, S, P, O, INFO)
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT32, INT64
  IMPLICIT NONE

  INTERFACE
     SUBROUTINE JSTRAT_INIT(JS, ID, N, INFO) BIND(C,NAME='jstrat_init_f')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
       IMPLICIT NONE
       INTEGER(KIND=INT64), INTENT(IN), TARGET :: ID, N
       INTEGER(KIND=INT64), INTENT(OUT), TARGET :: JS(*), INFO
     END SUBROUTINE JSTRAT_INIT
  END INTERFACE
  INTERFACE
     SUBROUTINE JSTRAT_NEXT_NC(JS, ARR, INFO) BIND(C,NAME='jstrat_next_f')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
       IMPLICIT NONE
       INTEGER(KIND=INT64), INTENT(INOUT), TARGET :: JS(*)
       INTEGER(KIND=INT64), INTENT(OUT), TARGET :: ARR(2,*), INFO
     END SUBROUTINE JSTRAT_NEXT_NC
  END INTERFACE
  INTERFACE
     SUBROUTINE JSTRAT_FREE(JS) BIND(C,NAME='jstrat_free_f')
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64
       IMPLICIT NONE
       INTEGER(KIND=INT64), INTENT(INOUT), TARGET :: JS(*)
     END SUBROUTINE JSTRAT_FREE
  END INTERFACE

  INTEGER(KIND=INT64), PARAMETER :: JSMLEX = 8_INT64
  INTEGER(KIND=INT64), PARAMETER :: JSROWC = 0_INT64
  INTEGER(KIND=INT64), PARAMETER :: JSCOLC = 1_INT64
  INTEGER(KIND=INT64), PARAMETER :: JSMENC = 2_INT64
  INTEGER(KIND=INT64), PARAMETER :: JSMMNC = 4_INT64

  INTEGER, INTENT(IN) :: J, N
  INTEGER, INTENT(OUT) :: S, P, O(2,*), INFO

  INTEGER(KIND=INT64), ALLOCATABLE :: JSARR(:,:)
  INTEGER(KIND=INT64) :: JS(JSMLEX), NPAIRS, NSTEPS, I, M
  INTEGER(KIND=INT32) :: C(2)
  EQUIVALENCE (M,C)
  INTEGER :: K, L

  INFO = 0
  S = 0
  P = 0
  IF (N .LT. 0) INFO = -2
  IF (J .LT. 0) INFO = -1
  IF (INFO .NE. 0) RETURN
  IF (N .LE. 1) RETURN

  I = INT(J,INT64)
  SELECT CASE (I)
  CASE (JSROWC, JSCOLC)
     NPAIRS = 1_INT64
  CASE (JSMENC, JSMMNC)
     INFO = -MOD(N,2) * 2
     IF (INFO .EQ. 0) NPAIRS = INT(N/2,INT64)
  CASE DEFAULT
     INFO = -1
  END SELECT
  IF (INFO .NE. 0) RETURN
  P = INT(NPAIRS)

  M = INT(N,INT64)
  CALL JSTRAT_INIT(JS, I, M, NSTEPS)
  IF (NSTEPS .LT. 0_INT64) THEN
     INFO = -3
     RETURN
  END IF
  JS(INT(JSMLEX)-1) = NSTEPS
  JS(INT(JSMLEX)) = NPAIRS
  IF (NSTEPS .GT. INT(HUGE(S),INT64)) THEN
     INFO = -3
     GOTO 1
  END IF
  S = INT(NSTEPS)

  ALLOCATE(JSARR(2,P),STAT=L)
  IF (L .GT. 0) THEN
     INFO = -4
     GOTO 1
  END IF

  L = 1
  DO I = 1, NSTEPS
     CALL JSTRAT_NEXT_NC(JS, JSARR, M)
     SELECT CASE (JS(1))
     CASE (JSROWC, JSCOLC)
        IF (M .NE. 1_INT64) THEN
           INFO = -6
        ELSE ! OK
           O(1,L) = INT(JSARR(1,1)) + 1
           O(2,L) = INT(JSARR(2,1)) + 1
           L = L + 1
        END IF
     CASE (JSMENC)
        IF (M .NE. NPAIRS) THEN
           INFO = -6
        ELSE ! OK
           DO K = 1, P
              O(1,L) = INT(JSARR(1,K)) + 1
              O(2,L) = INT(JSARR(2,K)) + 1
              L = L + 1
           END DO
        END IF
     CASE (JSMMNC)
        IF (-M .NE. NPAIRS) THEN
           INFO = -6
        ELSE ! OK
           DO K = 1, P
              M = JSARR(1,K)
              O(1,L) = INT(C(1)) + 1
              M = JSARR(2,K)
              O(2,L) = INT(C(1)) + 1
              L = L + 1
           END DO
        END IF
     CASE DEFAULT
        INFO = -5
     END SELECT
     IF (INFO .NE. 0) EXIT
  END DO
  DEALLOCATE(JSARR)

1 CALL JSTRAT_FREE(JS)
END SUBROUTINE JSWEEP
