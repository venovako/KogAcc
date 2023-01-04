  ! This is the generic part of the xLASV2 tester routines.
  ! The output is the same as that of tksvd2.f90, but the interactive input is adapted for upper triangular matrices.
  ONCE = .TRUE.
  SELECT CASE (COMMAND_ARGUMENT_COUNT())
  CASE (0)
     WRITE (*,'(A)',ADVANCE='NO') 'G(1,1)='
     READ (*,*) G(1,1)
     WRITE (*,'(A)',ADVANCE='NO') 'G(1,2)='
     READ (*,*) G(1,2)
     WRITE (*,'(A)',ADVANCE='NO') 'G(2,2)='
     READ (*,*) G(2,2)
  CASE (1)
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=INFO)
     IF ((INFO .NE. 0) .OR. (LEN_TRIM(CLA) .LE. 0)) ERROR STOP 'the input file name is invalid'
     IF ((LEN_TRIM(CLA) .EQ. 1) .AND. (CLA(1:1) .EQ. '-')) THEN
        I = INPUT_UNIT
     ELSE
        I = 1
     END IF
     IF (I .NE. INPUT_UNIT) OPEN(UNIT=I, FILE=TRIM(CLA), ACTION='READ', STATUS='OLD', IOSTAT=INFO)
     IF (INFO .NE. 0) ERROR STOP 'cannot open the input file'
     ONCE = .FALSE.
  CASE (3)
     CALL GET_COMMAND_ARGUMENT(1, CLA, STATUS=INFO)
     IF (INFO .NE. 0) ERROR STOP 'the first argument is invalid'
     READ (CLA,*) G(1,1)
     CALL GET_COMMAND_ARGUMENT(2, CLA, STATUS=INFO)
     IF (INFO .NE. 0) ERROR STOP 'the second argument is invalid'
     READ (CLA,*) G(1,2)
     CALL GET_COMMAND_ARGUMENT(3, CLA, STATUS=INFO)
     IF (INFO .NE. 0) ERROR STOP 'the third argument is invalid'
     READ (CLA,*) G(2,2)
  CASE DEFAULT
     ERROR STOP 'zero, one [input file name], or three [G(1,1) G(1,2) G(2,2)] arguments required'
  END SELECT
  DO WHILE (.TRUE.)
     IF (.NOT. ONCE) THEN
        ! read G(2,1) for compatibility with tksvd2 but it has to be zero
        READ (I,*,IOSTAT=INFO) G(1,1), G(1,2), G(2,1), G(2,2)
        IF (INFO .NE. 0) EXIT
     END IF
     WRITE (*,1) 'G(1,1)=', G(1,1)
     WRITE (*,1) 'G(2,1)=', G(2,1)
     WRITE (*,1) 'G(1,2)=', G(1,2)
     WRITE (*,1) 'G(2,2)=', G(2,2)
     CALL LSVD2(G, U, V, S, INFO)
     WRITE (*,1) 'U(1,1)=', U(1,1)
     WRITE (*,1) 'U(2,1)=', U(2,1)
     WRITE (*,1) 'U(1,2)=', U(1,2)
     WRITE (*,1) 'U(2,2)=', U(2,2)
     WRITE (*,1) 'V(1,1)=', V(1,1)
     WRITE (*,1) 'V(2,1)=', V(2,1)
     WRITE (*,1) 'V(1,2)=', V(1,2)
     WRITE (*,1) 'V(2,2)=', V(2,2)
     WRITE (*,1) 'S(1)=', S(1)
     WRITE (*,1) 'S(2)=', S(2)
     IF (INFO .EQ. IERR) THEN
        WRITE (*,'(A)') 'INFO=ERROR'
     ELSE ! all OK
        WRITE (*,2) 'INFO=', INFO
     END IF
     UX(1,1) = U(1,1)
     UX(2,1) = U(2,1)
     UX(1,2) = U(1,2)
     UX(2,2) = U(2,2)
     SX(1,1) = 1.0_KX
     SX(2,1) = 0.0_KX
     SX(1,2) = 0.0_KX
     SX(2,2) = 1.0_KX
     VX = MATMUL(TRANSPOSE(UX), UX) - SX
     WRITE (*,1) '||U^T U - I||_F=', CR_HYPOT(CR_HYPOT(VX(1,1), VX(2,1)), CR_HYPOT(VX(1,2), VX(2,2)))
     VX(1,1) = V(1,1)
     VX(2,1) = V(2,1)
     VX(1,2) = V(1,2)
     VX(2,2) = V(2,2)
     GX = MATMUL(TRANSPOSE(VX), VX) - SX
     WRITE (*,1) '||V^T V - I||_F=', CR_HYPOT(CR_HYPOT(GX(1,1), GX(2,1)), CR_HYPOT(GX(1,2), GX(2,2)))
     GX(1,1) = G(1,1)
     GX(2,1) = G(2,1)
     GX(1,2) = G(1,2)
     GX(2,2) = G(2,2)
     SX(1,1) = S(1)
     WRITE (*,1) 'SIGMA(1)=', SX(1,1)
     SX(2,2) = S(2)
     WRITE (*,1) 'SIGMA(2)=', SX(2,2)
     UX = MATMUL(MATMUL(UX, SX), TRANSPOSE(VX)) - GX
     SX(2,1) = CR_HYPOT(CR_HYPOT(UX(1,1), UX(2,1)), CR_HYPOT(UX(1,2), UX(2,2)))
     SX(1,2) = CR_HYPOT(CR_HYPOT(GX(1,1), GX(2,1)), CR_HYPOT(GX(1,2), GX(2,2)))
     IF ((SX(1,2) .EQ. 0.0_KX) .AND. (SX(2,1) .EQ. 0.0_KX)) THEN
        SX(2,1) = 0.0_KX
     ELSE ! the general case
        SX(2,1) = SX(2,1) / SX(1,2)
     END IF
     WRITE (*,1) '||U SIGMA V^T - G||_F / ||G||_F=', SX(2,1)
     IF (ONCE) EXIT
  END DO
  IF (.NOT. ONCE) THEN
     IF (I .NE. INPUT_UNIT) THEN
        CLOSE(UNIT=I, IOSTAT=INFO)
     ELSE
        INFO = 0
     END IF
     IF (INFO .NE. 0) ERROR STOP 'cannot close the input file'
  END IF
