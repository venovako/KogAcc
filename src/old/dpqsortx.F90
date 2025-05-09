!>@brief \b DPQSORTX tests the DPQSRT subroutine with parallel execution if N>0 or sequentially if N<0.
PROGRAM DPQSORTX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64, OUTPUT_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE
  INTERFACE
     PURE SUBROUTINE DPQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
       REAL(KIND=REAL64), INTENT(IN) :: XW, YW
       INTEGER, INTENT(OUT) :: INFO
     END SUBROUTINE DPQCMP
  END INTERFACE
  INTERFACE
     SUBROUTINE DPQSRT(WPQCMP, N, AW, AP, AQ, BW, BP, BQ, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
       IMPLICIT NONE
       ABSTRACT INTERFACE
          PURE SUBROUTINE PQCMP(XW, XP, XQ, YW, YP, YQ, INFO)
            USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL64
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: XP, XQ, YP, YQ
            REAL(KIND=REAL64), INTENT(IN) :: XW, YW
            INTEGER, INTENT(OUT) :: INFO
          END SUBROUTINE PQCMP
       END INTERFACE
       INTEGER, INTENT(IN) :: N
       REAL(KIND=REAL64), INTENT(INOUT) :: AW(N)
       INTEGER, INTENT(INOUT) :: AP(N), AQ(N), INFO
       REAL(KIND=REAL64), INTENT(OUT) :: BW(N)
       INTEGER, INTENT(OUT) :: BP(N), BQ(N)
       PROCEDURE(PQCMP) :: WPQCMP
     END SUBROUTINE DPQSRT
  END INTERFACE

  REAL(KIND=REAL64), ALLOCATABLE :: W(:)
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
  CALL DPQSRT(DPQCMP, M, W, P, Q, W(M+1), P(M+1), Q(M+1), INFO)
  WRITE (OUTPUT_UNIT,'(A,I11)') 'INFO = ', INFO
  IF (INFO .GE. 0) THEN
     DO I = 1, M
        WRITE (OUTPUT_UNIT,1) '[W,P,Q](', M, ') = [', W(I), ',', P(I), ',', Q(I), ']'
     END DO
  END IF
  DEALLOCATE(Q)
  DEALLOCATE(P)
  DEALLOCATE(W)
1 FORMAT(A,I2,A,ES25.17E3,A,I11,A,I11,A)
END PROGRAM DPQSORTX
