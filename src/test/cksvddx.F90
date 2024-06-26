!>@brief \b CKSVDDX tests the CKSVDD routine.
PROGRAM CKSVDDX
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: INT64, REAL32, REAL64, REAL128, ERROR_UNIT, OUTPUT_UNIT
  !$ USE OMP_LIB
  IMPLICIT NONE

  INTERFACE
     PURE SUBROUTINE NB2M(N, B, M, INFO)
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: N, B
       INTEGER, INTENT(OUT) :: M, INFO
     END SUBROUTINE NB2M
  END INTERFACE
  INTERFACE
     SUBROUTINE CBRDG(M, N, G, LDG, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: M, N, LDG
       COMPLEX(KIND=REAL32), INTENT(OUT) :: G(LDG,M)
       INTEGER, INTENT(INOUT) :: INFO
     END SUBROUTINE CBRDG
  END INTERFACE
  INTERFACE
     SUBROUTINE CKSVDD(JOB, N, G, LDG, U, LDU, V, LDV, SV, W, D, O, INFO)
       USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: REAL32, REAL64, REAL128
       IMPLICIT NONE
       INTEGER, INTENT(IN) :: JOB, N, LDG, LDU, LDV
       COMPLEX(KIND=REAL32), INTENT(INOUT) :: G(LDG,N), U(LDU,N), V(LDV,N)
       REAL(KIND=REAL128), INTENT(OUT) :: SV(N)
       REAL(KIND=REAL32), INTENT(INOUT) :: W(*)
       REAL(KIND=REAL64), INTENT(OUT) :: D(*)
       INTEGER, INTENT(INOUT) :: O(2,*), INFO
     END SUBROUTINE CKSVDD
  END INTERFACE

  INTEGER, PARAMETER :: K = REAL32, CLAL = 256
  CHARACTER(LEN=CLAL) :: BN
  INTEGER(KIND=INT64) :: C0, C1, CR
  REAL(KIND=REAL128) :: T
  INTEGER :: JOB, M, N, LDG, LDU, LDV, INFO, I, J, L
  COMPLEX(KIND=K), ALLOCATABLE :: G(:,:)
  !DIR$ ATTRIBUTES ALIGN: 64:: G
  COMPLEX(KIND=K), ALLOCATABLE :: U(:,:)
  !DIR$ ATTRIBUTES ALIGN: 64:: U
  COMPLEX(KIND=K), ALLOCATABLE :: V(:,:)
  !DIR$ ATTRIBUTES ALIGN: 64:: V
  REAL(KIND=K), ALLOCATABLE :: W(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: W
  REAL(KIND=REAL64), ALLOCATABLE :: D(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: D
  REAL(KIND=REAL128), ALLOCATABLE :: SV(:)
  !DIR$ ATTRIBUTES ALIGN: 64:: SV
  INTEGER, ALLOCATABLE :: O(:,:)
  !DIR$ ATTRIBUTES ALIGN: 64:: O

  I = COMMAND_ARGUMENT_COUNT()
  IF (I .NE. 2) THEN
     CALL GET_COMMAND_ARGUMENT(0, BN)
     WRITE (ERROR_UNIT,*) TRIM(BN), ' N BN'
     ERROR STOP 'INVALID COMMAND LINE'
  END IF
  CALL GET_COMMAND_ARGUMENT(1, BN, I, INFO)
  IF (INFO .NE. 0) ERROR STOP 'N'
  READ (BN,*) N
  CALL GET_COMMAND_ARGUMENT(2, BN, I, INFO)
  IF (INFO .NE. 0) ERROR STOP 'BN'

  L = 0
  !$ L = 1
  IF (N .EQ. 0) ERROR STOP 'N'
  IF (N .LT. 0) THEN
     N = -N
     I = -1
  ELSE ! the default mode is "slow"
     I = 0
  END IF
  M = N + MOD(N, 2)
  LDV = 32
  LDU = LDV / INT(SIZEOF(0.0_K))
  LDG = MOD(M, LDU)
  IF (LDG .NE. 0) M = M + (LDU - LDG)
  LDG = M
  LDU = M
  LDV = M

  ALLOCATE(U(LDU,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL CBRDG(M, N, U, LDU, INFO)
     IF (INFO .NE. 0) ERROR STOP 'CBRDG(U)'
  END IF

  ALLOCATE(V(LDV,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL CBRDG(M, N, V, LDV, INFO)
     IF (INFO .NE. 0) ERROR STOP 'CBRDG(V)'
  END IF

  ALLOCATE(G(LDG,M))
  IF (M .GT. N) THEN
     INFO = L
     CALL CBRDG(M, N, G, LDG, INFO)
     IF (INFO .NE. 0) ERROR STOP 'CBRDG(G)'
  END IF

  INFO = L
  CALL CRDINP(BN, M, N, G, LDG, INFO)
  IF (INFO .NE. 0) ERROR STOP 'CRDINP'

  ALLOCATE(SV(M))
  ALLOCATE(W(MAX((M-1),5)*M))
  ! if, e.g., ||G||_F is known to be numerically finite and reasonably below HUGE,
  ! the dynamic scaling can be turned off for speed
  W(1) = REAL(I, K)
  W(2) = 0.0_K
  W(3) = 0.0_K

  ALLOCATE(D((M*(M-1))/2))

  ALLOCATE(O(2,M*(M/2)))
  JOB = 1
  DO I = 1, M-1
     DO J = I+1, M
        O(1,JOB) = I
        O(2,JOB) = J
        JOB = JOB + 1
     END DO
  END DO
  J = 3
  JOB = 123
  INFO = -HUGE(INFO)
  INFO = INFO - 1
  !$ IF (L .NE. 0) INFO = -(INFO + 1)
  CALL SYSTEM_CLOCK(C0)
  CALL CKSVDD(JOB, M, G, LDG, U, LDU, V, LDV, SV, W, D, O, INFO)
  CALL SYSTEM_CLOCK(C1, CR)
  T = REAL(CR, REAL128)
  T = REAL(C1 - C0, REAL128) / T
  WRITE (OUTPUT_UNIT,'(A,F15.6,A,I11,A)',ADVANCE='NO') 'CKSVDD took ', T, ' s with ', INFO, ' steps and W=('
  WRITE (OUTPUT_UNIT,9) W(1), ',', W(2), ',', W(3), ')'
  FLUSH(OUTPUT_UNIT)
  IF (INFO .LT. 0) THEN
     WRITE (ERROR_UNIT,*) 'ERROR in CKSVDD'
  ELSE IF (INFO .EQ. HUGE(INFO)) THEN
     WRITE (ERROR_UNIT,*) 'NO CONVERGENCE'
  ELSE ! all OK
     WRITE (ERROR_UNIT,*) 'output basename: ', TRIM(BN)
  END IF

  DEALLOCATE(O)
  DEALLOCATE(D)
  DEALLOCATE(W)
  DEALLOCATE(G)

  INFO = L
  CALL CWROUT(BN, J, N, U, LDU, V, LDV, SV, INFO)
  IF (INFO .NE. 0) ERROR STOP 'CWROUT'

  DEALLOCATE(SV)
  DEALLOCATE(V)
  DEALLOCATE(U)

9 FORMAT(3(ES16.9E2,A))
CONTAINS
#include "crdinp.F90"
#include "cwrout.F90"
END PROGRAM CKSVDDX
