!>@brief \b XPQSORTX tests the XPQSORT subroutine if \f$N>0\f$ or the XPQSRT subroutine if \f$N<0\f$.
PROGRAM XPQSORTX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  IMPLICIT NONE
  REAL(KIND=10), ALLOCATABLE :: W(:)
  INTEGER, ALLOCATABLE :: P(:), Q(:)
  INTEGER :: I, M, N, INFO
  EXTERNAL :: XPQCMP, XPQSORT, XPQSRT
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
  IF (N .GT. 0) THEN
     CALL XPQSORT(N, W, P, Q, W(N+1), P(N+1), Q(N+1), INFO)
  ELSE ! N .LT. 0
     CALL XPQSRT(M, W, P, Q, W(M+1), P(M+1), Q(M+1), INFO)
  END IF
  WRITE (OUTPUT_UNIT,'(A,I11)') 'INFO = ', INFO
  IF (INFO .GE. 0) THEN
     DO I = 1, M
        WRITE (OUTPUT_UNIT,1) '[W,P,Q](', M, ') = [', W(I), ',', P(I), ',', Q(I), ']'
     END DO
  END IF
  DEALLOCATE(Q)
  DEALLOCATE(P)
  DEALLOCATE(W)
1 FORMAT(A,I2,A,ES30.21E4,A,I11,A,I11,A)
END PROGRAM XPQSORTX