!>@brief \b XPQSORTX tests the XPQSRT subroutine with parallel execution if N>0 or sequentially N<0.
PROGRAM XPQSORTX
  USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE

  INTERFACE
     PURE SUBROUTINE XPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
       REAL(KIND=c_long_double), INTENT(IN) :: XW, YW
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE XPQCMP
  END INTERFACE
  INTERFACE
     SUBROUTINE XPQSRT(WPQCMP, N, AW, AP, AQ, BW, BP, BQ, INFO)
       USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
       IMPLICIT NONE
       ABSTRACT INTERFACE
          PURE SUBROUTINE PQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY: c_long_double
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
            REAL(KIND=c_long_double), INTENT(IN) :: XW, YW
            INTEGER, INTENT(OUT) :: INFO
          END SUBROUTINE PQCMP
       END INTERFACE
       INTEGER, INTENT(IN) :: N
       REAL(KIND=c_long_double), INTENT(INOUT) :: AW(N)
       INTEGER, INTENT(INOUT) :: AP(N), AQ(N), INFO
       REAL(KIND=c_long_double), INTENT(OUT) :: BW(N)
       INTEGER, INTENT(OUT) :: BP(N), BQ(N)
       PROCEDURE(PQCMP) :: WPQCMP
     END SUBROUTINE XPQSRT
  END INTERFACE

  REAL(KIND=c_long_double), ALLOCATABLE :: W(:)
  INTEGER, ALLOCATABLE :: P(:), Q(:)
  INTEGER :: I, M, N, INFO

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
  CALL XPQSRT(XPQCMP, M, W, P, Q, W(M+1), P(M+1), Q(M+1), INFO)
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
