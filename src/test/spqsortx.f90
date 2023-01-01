!>@brief \b SPQSORTX tests the SPQSRT subroutine with parallel execution if \f$N>0\f$ or sequentially if \f$N<0\f$.
PROGRAM SPQSORTX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, OUTPUT_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE
  REAL(KIND=REAL32), ALLOCATABLE :: W(:)
  INTEGER, ALLOCATABLE :: P(:), Q(:)
  INTEGER :: I, M, N, INFO
  EXTERNAL :: SPQCMP, SPQSRT
  WRITE (OUTPUT_UNIT,'(A)',ADVANCE='NO') 'N = '
  READ (*,*) N
  IF (N .EQ. 0) STOP 'N = 0'
  M = ABS(N)
  I = 2 * M
  ALLOCATE(W(I))
  ALLOCATE(P(I))
  ALLOCATE(Q(I))
  DO I = 1, M
     WRITE (OUTPUT_UNIT,'(A,I2,A)',ADVANCE='NO') '[W,P,Q](', M, ') = '
     READ (*,*) W(I), P(I), Q(I)
  END DO
  INFO = 0
  IF (N .GT. 0) THEN
     !$ INFO = OMP_GET_NUM_THREADS()
     CONTINUE
  END IF
  CALL SPQSRT(SPQCMP, M, W, P, Q, W(M+1), P(M+1), Q(M+1), INFO)
  WRITE (OUTPUT_UNIT,'(A,I11)') 'INFO = ', INFO
  IF (INFO .GE. 0) THEN
     DO I = 1, M
        WRITE (OUTPUT_UNIT,1) '[W,P,Q](', M, ') = [', W(I), ',', P(I), ',', Q(I), ']'
     END DO
  END IF
  DEALLOCATE(Q)
  DEALLOCATE(P)
  DEALLOCATE(W)
1 FORMAT(A,I2,A,ES16.9E2,A,I11,A,I11,A)
END PROGRAM SPQSORTX
