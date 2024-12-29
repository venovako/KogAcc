!>@brief \b JSWEEPX tests the JSWEEP subroutine.
PROGRAM JSWEEPX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT, OUTPUT_UNIT
  IMPLICIT NONE
  INTERFACE
     SUBROUTINE JSWEEP(J, N, S, P, O, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: J, N
       INTEGER, INTENT(OUT) :: S, P, O(2,*), INFO
     END SUBROUTINE JSWEEP
  END INTERFACE

  CHARACTER(LEN=256) :: V
  INTEGER, ALLOCATABLE :: O(:,:)
  INTEGER :: I, J, K, L, N, S, P, INFO

  IF (COMMAND_ARGUMENT_COUNT() .NE. 2) THEN
     CALL GET_COMMAND_ARGUMENT(0, V, STATUS=INFO)
     IF (INFO .EQ. 0) THEN
        WRITE (ERROR_UNIT,'(A,A)') TRIM(V), ' J N'
     ELSE ! truncation
        WRITE (ERROR_UNIT,'(A)') 'jsweep.exe J N'
     END IF
     STOP 'argument count mismatch'
  END IF

  CALL GET_COMMAND_ARGUMENT(1, V, STATUS=INFO)
  IF (INFO .NE. 0) STOP 'J'
  READ (V,*) J
  IF (J .LT. 0) STOP 'J < 0'

  CALL GET_COMMAND_ARGUMENT(2, V, STATUS=INFO)
  IF (INFO .NE. 0) STOP 'N'
  READ (V,*) N
  IF (N .LT. 0) STOP 'N < 0'
  IF (N .GE. 100000) STOP 'N >= 100000'

  ALLOCATE(O(2,N*(N-1)))
  CALL JSWEEP(J, N, S, P, O, INFO)
  IF (INFO .NE. 0) THEN
     WRITE (ERROR_UNIT,'(A,I2)') 'INFO=', INFO
  ELSE ! OK
     WRITE (ERROR_UNIT,*) 'J=', J, 'N=', N, 'S=', S, 'P=', P
     L = 1
     IF (N .LT. 10) THEN
        DO I = 1, S
           DO K = 1, P-1
              WRITE (OUTPUT_UNIT,1,ADVANCE='NO') O(1,L), O(2,L)
              WRITE (OUTPUT_UNIT,'(A)',ADVANCE='NO') ','
              L = L + 1
           END DO
           WRITE (OUTPUT_UNIT,1) O(1,L), O(2,L)
           L = L + 1
        END DO
     ELSE IF (N .LT. 100) THEN
        DO I = 1, S
           DO K = 1, P-1
              WRITE (OUTPUT_UNIT,2,ADVANCE='NO') O(1,L), O(2,L)
              WRITE (OUTPUT_UNIT,'(A)',ADVANCE='NO') ','
              L = L + 1
           END DO
           WRITE (OUTPUT_UNIT,2) O(1,L), O(2,L)
           L = L + 1
        END DO
     ELSE IF (N .LT. 1000) THEN
        DO I = 1, S
           DO K = 1, P-1
              WRITE (OUTPUT_UNIT,3,ADVANCE='NO') O(1,L), O(2,L)
              WRITE (OUTPUT_UNIT,'(A)',ADVANCE='NO') ','
              L = L + 1
           END DO
           WRITE (OUTPUT_UNIT,3) O(1,L), O(2,L)
           L = L + 1
        END DO
     ELSE IF (N .LT. 10000) THEN
        DO I = 1, S
           DO K = 1, P-1
              WRITE (OUTPUT_UNIT,4,ADVANCE='NO') O(1,L), O(2,L)
              WRITE (OUTPUT_UNIT,'(A)',ADVANCE='NO') ','
              L = L + 1
           END DO
           WRITE (OUTPUT_UNIT,4) O(1,L), O(2,L)
           L = L + 1
        END DO
     ELSE ! N < 100000
        DO I = 1, S
           DO K = 1, P-1
              WRITE (OUTPUT_UNIT,5,ADVANCE='NO') O(1,L), O(2,L)
              WRITE (OUTPUT_UNIT,'(A)',ADVANCE='NO') ','
              L = L + 1
           END DO
           WRITE (OUTPUT_UNIT,5) O(1,L), O(2,L)
           L = L + 1
        END DO
     END IF
  END IF
  DEALLOCATE(O)

1 FORMAT('(',I1,',',I1,')')
2 FORMAT('(',I2,',',I2,')')
3 FORMAT('(',I3,',',I3,')')
4 FORMAT('(',I4,',',I4,')')
5 FORMAT('(',I5,',',I5,')')
END PROGRAM JSWEEPX
